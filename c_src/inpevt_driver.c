/*! @file inpevt_driver.c

    Copyright (C) 2011, Feuerlabs, Inc. All rights reserved.
    Redistribution and use in any form, with or without modification, is strictly prohibited.
*/

#include "erl_driver.h"
#include <linux/input.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include "dterm.h"


#define IPEVT_DRV_MAJOR_VER 1
#define IPEVT_DRV_MINOR_VER 0


typedef struct {
    ErlDrvPort mPort;
    ErlDrvTermData mDport;
    int mDescriptor; // Opened mDevice
    char mDevice[256]; // /dev/input/...
    char mActive;  // Are events reported or not?
} IEContext;



static int inpevt_init (void);
static ErlDrvData inpevt_start (ErlDrvPort port, char *command);

static void inpevt_stop (ErlDrvData drv_data);

static ErlDrvSSizeT inpevt_control(ErlDrvData drv_data,
                                   unsigned int command,
                                   char *buf,
                                   ErlDrvSizeT len,
                                   char **rbuf,
                                   ErlDrvSizeT rlen);

static void inpevt_ready_input(ErlDrvData drv_data, ErlDrvEvent event);
static unsigned char open_event_device(IEContext* ctx);

static unsigned char send_device_info(IEContext* ctx,
                                      unsigned int reply_id);

static ErlDrvSSizeT port_ctl_return_val(unsigned char code,
                                        unsigned int arg,
                                        char* resbuf);

static unsigned char add_cap(dterm_t* dt, int fd);

static unsigned char add_sync_cap(dterm_t* dt, int fd);
static unsigned char add_rel_cap(dterm_t* dt, int fd);
static unsigned char add_key_cap(dterm_t* dt, int fd);

static ErlDrvEntry inpevt_driver_entry = {
    inpevt_init,                 // init
    inpevt_start,
    inpevt_stop,
    NULL,                        // output
    inpevt_ready_input,          // ready_input
    NULL,                        // ready_output
    "inpevt_driver",             // the name of the driver
    NULL,                        // finish
    NULL,                        // handle
    inpevt_control,
    NULL,                        // timeout
    NULL,                        // outputv
    NULL,                        // ready_async
    NULL,                        // flush
    NULL,                        // call
    NULL,                        // event
    ERL_DRV_EXTENDED_MARKER,     // Extended
    ERL_DRV_EXTENDED_MAJOR_VERSION, // Driver major version
    ERL_DRV_EXTENDED_MINOR_VERSION, // Driver minor version
    0,                           // Driver flags
    NULL,                        // handle2
    NULL,                        // process exit
    NULL                         // stop_select
};

#define IEDRV_CMD_MASK  0x0000000F

#define IEDRV_CMD_OPEN  0x00000001
#define IEDRV_CMD_CLOSE 0x00000002
#define IEDRV_CMD_ACTIVATE 0x00000003
#define IEDRV_CMD_DEACTIVATE 0x00000004
#define IEDRV_CMD_GET_EVENT_VERSION 0x00000005
#define IEDRV_CMD_GET_BUS_TYPE 0x00000003

#define IEDRV_RES_MASK 0xFF;
#define IEDRV_RES_OK 0
#define IEDRV_RES_IO_ERROR 1
#define IEDRV_RES_NOT_OPEN 2
#define IEDRV_RES_ILLEGAL_ARG 3

#define LONG_BITS (sizeof(long) * sizeof(char))
#define NLONGS(x) (((x) + LONG_BITS - 1) / LONG_BITS)

#define MAX_REPLY_ID 0x00FFFFFF

static ErlDrvTermData ie_key;
static ErlDrvTermData ie_relative;
static ErlDrvTermData ie_sync;
static ErlDrvTermData ie_id;
static ErlDrvTermData ie_version;
static ErlDrvTermData ie_topology;
static ErlDrvTermData ie_capability;
static ErlDrvTermData ie_name;
static ErlDrvTermData ie_device_info;
static ErlDrvTermData ie_unknown;
static ErlDrvTermData ie_bus_type;
static ErlDrvTermData ie_pci;
static ErlDrvTermData ie_bus_type;
static ErlDrvTermData ie_pci;
static ErlDrvTermData ie_isapnp;
static ErlDrvTermData ie_usb;
static ErlDrvTermData ie_hil;
static ErlDrvTermData ie_bluetooth;
static ErlDrvTermData ie_virtual;
static ErlDrvTermData ie_isa;
static ErlDrvTermData ie_i8042;
static ErlDrvTermData ie_xtkbd;
static ErlDrvTermData ie_rs232;
static ErlDrvTermData ie_gameport;
static ErlDrvTermData ie_parport;
static ErlDrvTermData ie_amiga;
static ErlDrvTermData ie_adb;
static ErlDrvTermData ie_i2c;
static ErlDrvTermData ie_host;
static ErlDrvTermData ie_gsc;
static ErlDrvTermData ie_atari;
static ErlDrvTermData ie_spi;


struct {
    ErlDrvTermData* mAtom;
    __u16 mBusType;
} mBusTypeMap[] = {
    { &ie_pci, BUS_PCI },
    { &ie_isapnp, BUS_ISAPNP },
    { &ie_usb, BUS_USB },
    { &ie_hil, BUS_HIL },
    { &ie_bluetooth, BUS_BLUETOOTH },
    { &ie_virtual, BUS_VIRTUAL },
    { &ie_isa, BUS_ISA },
    { &ie_i8042, BUS_I8042 },
    { &ie_xtkbd, BUS_XTKBD },
    { &ie_rs232, BUS_RS232 },
    { &ie_gameport, BUS_GAMEPORT },
    { &ie_parport, BUS_PARPORT },
    { &ie_amiga, BUS_AMIGA },
    { &ie_adb, BUS_ADB },
    { &ie_i2c, BUS_I2C },
    { &ie_host, BUS_HOST },
    { &ie_gsc, BUS_GSC },
    { &ie_atari, BUS_ATARI },
    { &ie_spi, BUS_SPI }
};

DRIVER_INIT(inpevt_driver)
{
    return &inpevt_driver_entry;
}

static void setup_atoms(void)
{
    ie_key = driver_mk_atom("key");
    ie_sync = driver_mk_atom("sync");
    ie_relative = driver_mk_atom("relative");
    ie_id = driver_mk_atom("id");
    ie_version = driver_mk_atom("version");
    ie_topology = driver_mk_atom("topology");
    ie_capability = driver_mk_atom("capability");
    ie_name = driver_mk_atom("name");
    ie_device_info = driver_mk_atom("device_info");
    ie_bus_type = driver_mk_atom("bus_type");
    ie_unknown = driver_mk_atom("unknown");

    ie_pci = driver_mk_atom("pci");
    ie_bus_type = driver_mk_atom("bus_type");
    ie_pci = driver_mk_atom("pci");
    ie_isapnp = driver_mk_atom("isapnp");
    ie_usb = driver_mk_atom("usb");
    ie_hil = driver_mk_atom("hil");
    ie_bluetooth = driver_mk_atom("bluetooth");
    ie_virtual = driver_mk_atom("virtual");
    ie_isa = driver_mk_atom("isa");
    ie_i8042 = driver_mk_atom("i8042");
    ie_xtkbd = driver_mk_atom("xtkbd");
    ie_rs232 = driver_mk_atom("rs232");
    ie_gameport = driver_mk_atom("gameport");
    ie_parport = driver_mk_atom("parport");
    ie_amiga = driver_mk_atom("amiga");
    ie_adb = driver_mk_atom("adb");
    ie_i2c = driver_mk_atom("i2c");
    ie_host = driver_mk_atom("host");
    ie_gsc = driver_mk_atom("gsc");
    ie_atari = driver_mk_atom("atari");
    ie_spi = driver_mk_atom("spi");
}

static ErlDrvData inpevt_start(ErlDrvPort port, char *command)
{
    IEContext *ctx = 0;

    ctx = (IEContext*) driver_alloc(sizeof(IEContext));
    ctx->mDescriptor = -1;
    ctx->mPort = port;
    ctx->mDport = driver_mk_port(port);
    ctx->mActive = 0;
    ctx->mDevice[0] = 0;
//    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    set_port_control_flags(port, 0);
    setup_atoms();
    return (ErlDrvData) ctx;
}


static int inpevt_init(void)
{
    return 0;
}
static void inpevt_stop (ErlDrvData drv_data)
{
    driver_free(drv_data);
}

/*
 * buf contains port number to open.
 */
static ErlDrvSSizeT inpevt_control (ErlDrvData drv_data,
                                    unsigned int command,
                                    char *buf,
                                    ErlDrvSizeT len,
                                    char **rbuf,
                                    ErlDrvSizeT rlen)
{
    IEContext* ctx = 0;
    unsigned int ev_ver = 0;
    static unsigned int reply_id = 0;
    unsigned char res = 0;

    ctx = (IEContext*) drv_data;

    switch(command & IEDRV_CMD_MASK) {
    case IEDRV_CMD_OPEN:
    {
        // Make a stack copy of buf so that we can add a null.
        if (len > sizeof(ctx->mDevice) - 1) {
            // Avoid overflow.
            memcpy(ctx->mDevice, buf, sizeof(ctx->mDevice) - 1);
            ctx->mDevice[sizeof(ctx->mDevice)-1] = 0;
        } else {
            memcpy(ctx->mDevice, buf, len);
            ctx->mDevice[len] = 0;
        }

        printf("Will open input event file [%s]\r\n", ctx->mDevice);

        if ((res = open_event_device(ctx)) != IEDRV_RES_OK  ||
            (res = send_device_info(ctx, reply_id)) != IEDRV_RES_OK)
            return port_ctl_return_val(res, 0, *rbuf);

        return port_ctl_return_val(IEDRV_RES_OK, reply_id++, *rbuf);
    }

    case IEDRV_CMD_ACTIVATE:
        if (ctx->mDescriptor == -1) {
            **rbuf = IEDRV_RES_NOT_OPEN;
            return 1;
        }
        ctx->mActive = 1;
        driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 1);
        **rbuf = IEDRV_RES_OK;
        return 1;


    case IEDRV_CMD_DEACTIVATE:
        if (ctx->mDescriptor == -1) {
            **rbuf = IEDRV_RES_NOT_OPEN;
            return 1;
        }
        if (ctx->mActive == 1) {
            ctx->mActive = 0;
            driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 0);
        }
        **rbuf = IEDRV_RES_OK;
        return 1;


    case IEDRV_CMD_GET_EVENT_VERSION:
        if (ctx->mDescriptor == -1) {
            **rbuf = IEDRV_RES_NOT_OPEN;
            return 1;
        }
        if (rlen < sizeof(ev_ver))
            *rbuf = driver_alloc_binary((ErlDrvSizeT) (sizeof(ev_ver) + 1))->orig_bytes;

        if (ioctl(ctx->mDescriptor, EVIOCGVERSION, &ev_ver) == -1) {
            **rbuf = IEDRV_RES_IO_ERROR;
        }

        // Return [ok:8][version:32]
        **rbuf = IEDRV_RES_OK;
        memcpy((*rbuf) + 1, &ev_ver, sizeof(ev_ver));
        return 1 + sizeof(ev_ver);


    // Are we closing?
    case IEDRV_CMD_CLOSE:
        // Remove from select set.
        if (ctx->mDescriptor != -1 && ctx->mActive == 1)
            driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 0);

        close(ctx->mDescriptor);

        // FIXME: UNEXPORT
        ctx->mDescriptor = -1;
        ctx->mActive = 0;

        **rbuf = IEDRV_RES_OK;
        return 1;


    default:
        break;
    }

    puts("Illegal command\r");
    **rbuf = IEDRV_RES_ILLEGAL_ARG;
    return 1;
}


static void inpevt_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    IEContext* ctx = 0;
    char buf[256];
    ssize_t rd_res = 0;

    ctx = (IEContext*) drv_data;

    if (ctx->mDescriptor == -1) {
        printf("inpevt_ready_input(): File not open\n\r");
        return;
    }

    while((rd_res = read(ctx->mDescriptor, buf, sizeof(buf))) > 0) {
        printf("inpevt_ready_input(): Got [%d] bytes of data\n", rd_res);
//    driver_output(ctx->mPort, buf, rd_res);
    }

    return;
}


static unsigned char send_device_info(IEContext* ctx, unsigned int reply_id)
{
    dterm_mark_t msg;
    dterm_t dt;
    int i = 0;
    int len = 0;
    char name[256];
    char topology[256];
    char uniq_id[256];

    struct input_id id;

    // Get device id.
    if (ioctl(ctx->mDescriptor, EVIOCGID, &id) < 0)
    {
        printf("ioctl EVIOCGID failed: %s\n", strerror(errno));
        return IEDRV_RES_IO_ERROR;
    }

    if ((len = ioctl(ctx->mDescriptor, EVIOCGNAME(sizeof(name) - 1), name)) < 0) {
        printf("ioctl EVIOCGNAME failed: %s\n", strerror(errno));
        return IEDRV_RES_IO_ERROR;
    }
    name[len] = 0;

    if ((len = ioctl(ctx->mDescriptor, EVIOCGPHYS(sizeof(topology) - 1), topology)) < 0) {
        printf("ioctl EVIOCGPHYS failed: %s\n", strerror(errno));
        return IEDRV_RES_IO_ERROR;
    }
    topology[len] = 0;

    if ((len = ioctl(ctx->mDescriptor, EVIOCGUNIQ(sizeof(uniq_id) - 1), uniq_id)) < 0)
        uniq_id[0] = 0;

    uniq_id[len] = 0;



    // {
    //   inpevt_device_info,
    //   Port,
    //   reply_id,
    //   [
    //     { id, { Bustype, Vendor, Product, Version, Name } }
    //     ...
    //   ]
    // }
    dterm_init(&dt);

    dterm_tuple_begin(&dt, &msg); {
        dterm_mark_t prop_list;
        dterm_atom(&dt, ie_device_info);
        dterm_port(&dt, ctx->mDport);
        dterm_int(&dt, reply_id);
        dterm_list_begin(&dt, &prop_list); {
            dterm_mark_t prop;

            //
            // Setup { id, { Bustype, Vendor, Product, Version, Name }}
            //
            dterm_tuple_begin(&dt, &prop); {
                dterm_mark_t id_prop;

                dterm_atom(&dt, ie_id);

                dterm_tuple_begin(&dt, &id_prop); {
                    // Find the correct bus atom
                    for(i = 0; i < sizeof(mBusTypeMap) / sizeof(mBusTypeMap[0]); ++i)
                        if (mBusTypeMap[i].mBusType == id.bustype)
                            break;

                    // Did we find something?
                    if (i < sizeof(mBusTypeMap) / sizeof(mBusTypeMap[0]))
                        dterm_atom(&dt, *mBusTypeMap[i].mAtom);
                    else
                        dterm_atom(&dt, ie_unknown);

                    dterm_int(&dt, id.vendor);
                    dterm_int(&dt, id.product);
                    dterm_int(&dt, id.version);
                    dterm_string(&dt, uniq_id, strlen(uniq_id));
                    dterm_string(&dt, name, strlen(name));
                }
                dterm_tuple_end(&dt, &id_prop);
            }
            dterm_tuple_end(&dt, &prop);

            //
            // Setup { topology, Topology }
            //
            dterm_tuple_begin(&dt, &prop); {
                dterm_atom(&dt, ie_topology);
                dterm_string(&dt, topology, strlen(topology));
            }
            dterm_tuple_end(&dt, &prop);

            //
            // Setup { capability, [ { Cap, [X] }, { Cap, [Y] }, ...}
            //
            add_cap(&dt,  ctx->mDescriptor);

        }
        dterm_list_end(&dt, &prop_list);
    }
    dterm_tuple_end(&dt, &msg);

    driver_output_term(ctx->mPort, dterm_data(&dt), dterm_used_size(&dt));

    dterm_finish(&dt);

    return IEDRV_RES_OK;
}

static ErlDrvSSizeT port_ctl_return_val(unsigned char code, unsigned int arg, char* resbuf)
{
    unsigned int res = (code << 24) | arg;

    memcpy(resbuf, &res, sizeof(res));
    return sizeof(res);
}


static unsigned char open_event_device(IEContext* ctx)
{
    if (ctx->mDescriptor != -1) {
        close(ctx->mDescriptor);
        ctx->mDescriptor = -1;
    }

    ctx->mDescriptor = open(ctx->mDevice, O_RDONLY);
    if (ctx->mDescriptor == -1) {
        printf("Failed to open %s: %s\n\r", ctx->mDevice, strerror(errno));

        return IEDRV_RES_IO_ERROR;
    }

    return IEDRV_RES_OK;
}




    /* len = ioctl(fd, EVIOCGBIT(EV_ABS, sizeof(abs_bitmask)), abs_bitmask); */
    /* if (len < 0) { */
    /*     printf("ioctl EVIOCGBIT failed: %s\n", strerror(errno)); */
    /*     return IEDRV_RES_IO_ERROR; */
    /* } */

    /* len = ioctl(fd, EVIOCGBIT(EV_LED, sizeof(led_bitmask)), led_bitmask); */
    /* if (len < 0) { */
    /*     printf("ioctl EVIOCGBIT failed: %s\n", */
    /*                 strerror(errno)); */
    /*     return IEDRV_RES_IO_ERROR; */
    /* } */

    /*
     * Do not try to validate absinfo data since it is not expected
     * to be static, always refresh it in evdev structure.
     */
    /* for (i = ABS_X; i <= ABS_MAX; i++) { */
    /*     if (EvdevBitIsSet(abs_bitmask, i)) { */
    /*         len = ioctl(ctx->mDescriptor, EVIOCGABS(i), &pEvdev->absinfo[i]); */
    /*         if (len < 0) { */
    /*             printf("ioctl EVIOCGABSi(%d) failed: %s\n", */
    /*                         i, strerror(errno)); */
    /*             goto error; */
    /*         } */
    /*         printf("absolute axis %#x [%d..%d]\n", */
    /*                i, pEvdev->absinfo[i].maximum, pEvdev->absinfo[i].minimum); */
    /*     } */
    /* } */

    /* len = ioctl(fd, EVIOCGBIT(EV_KEY, sizeof(key_bitmask)), key_bitmask); */
    /* if (len < 0) { */
    /*     printf("ioctl EVIOCGBIT failed: %s\n", */
    /*                 strerror(errno)); */
    /*     return IEDRV_RES_IO_ERROR; */
    /* } */




static char t_bit(unsigned char* bitmask, int bitno)
{
    if (bitmask[bitno / 8] & (1 << (bitno % 8)))
        return 1;

    return 0;
}

static unsigned char add_cap(dterm_t* dt, int fd)
{
    dterm_mark_t prop;
    unsigned char master_bitmask[EV_MAX / 8 + 1];
    unsigned res = IEDRV_RES_OK;
    dterm_mark_t c_list;

    dterm_tuple_begin(dt, &prop);

    dterm_atom(dt, ie_capability);
    dterm_list_begin(dt, &c_list);

    // Retrieve maste capabilities.
    if (ioctl(fd, EVIOCGBIT(0, sizeof(master_bitmask)), master_bitmask) < 0) {
        printf("ioctl EVIOCGBIT failed: %s\n", strerror(errno));
        res = IEDRV_RES_IO_ERROR;
        goto end;
    }

    if (t_bit(master_bitmask, EV_SYN) &&
        (res = add_sync_cap(dt, fd)) != IEDRV_RES_OK)
        goto end;

    if (t_bit(master_bitmask, EV_REL) &&
        (res = add_rel_cap(dt, fd)) != IEDRV_RES_OK)
        goto end;

    if (t_bit(master_bitmask, EV_KEY) &&
        (res = add_key_cap(dt, fd)) != IEDRV_RES_OK)
        goto end;

end:
    dterm_list_end(dt, &c_list);
    dterm_tuple_end(dt, &prop);

    return res;
}

static unsigned char add_key_cap(dterm_t* dt, int fd)
{
    dterm_mark_t prop;
    dterm_mark_t c_list;
    unsigned char key_bitmask[KEY_CNT / 8 + 1];
    static struct {
        unsigned int bit;
        char* name;
        ErlDrvTermData atom;
    } cap_map[] = {
        { KEY_RESERVED, "reserved", 0 },
        { KEY_ESC, "esc", 0 },
        { KEY_1, "1", 0 },
        { KEY_2, "2", 0 },
        { KEY_3, "3", 0 },
        { KEY_4, "4", 0 },
        { KEY_5, "5", 0 },
        { KEY_6, "6", 0 },
        { KEY_7, "7", 0 },
        { KEY_8, "8", 0 },
        { KEY_9, "9", 0 },
        { KEY_0, "0", 0 },
        { KEY_MINUS, "minus", 0 },
        { KEY_EQUAL, "equal", 0 },
        { KEY_BACKSPACE, "backspace", 0 },
        { KEY_TAB, "tab", 0 },
        { KEY_Q, "q", 0 },
        { KEY_W, "w", 0 },
        { KEY_E, "e", 0 },
        { KEY_R, "r", 0 },
        { KEY_T, "t", 0 },
        { KEY_Y, "y", 0 },
        { KEY_U, "u", 0 },
        { KEY_I, "i", 0 },
        { KEY_O, "o", 0 },
        { KEY_P, "p", 0 },
        { KEY_LEFTBRACE, "leftbrace", 0 },
        { KEY_RIGHTBRACE, "rightbrace", 0 },
        { KEY_ENTER, "enter", 0 },
        { KEY_LEFTCTRL, "leftctrl", 0 },
        { KEY_A, "a", 0 },
        { KEY_S, "s", 0 },
        { KEY_D, "d", 0 },
        { KEY_F, "f", 0 },
        { KEY_G, "g", 0 },
        { KEY_H, "h", 0 },
        { KEY_J, "j", 0 },
        { KEY_K, "k", 0 },
        { KEY_L, "l", 0 },
        { KEY_SEMICOLON, "semicolon", 0 },
        { KEY_APOSTROPHE, "apostrophe", 0 },
        { KEY_GRAVE, "grave", 0 },
        { KEY_LEFTSHIFT, "leftshift", 0 },
        { KEY_BACKSLASH, "backslash", 0 },
        { KEY_Z, "z", 0 },
        { KEY_X, "x", 0 },
        { KEY_C, "c", 0 },
        { KEY_V, "v", 0 },
        { KEY_B, "b", 0 },
        { KEY_N, "n", 0 },
        { KEY_M, "m", 0 },
        { KEY_COMMA, "comma", 0 },
        { KEY_DOT, "dot", 0 },
        { KEY_SLASH, "slash", 0 },
        { KEY_RIGHTSHIFT, "rightshift", 0 },
        { KEY_KPASTERISK, "kpasterisk", 0 },
        { KEY_LEFTALT, "leftalt", 0 },
        { KEY_SPACE, "space", 0 },
        { KEY_CAPSLOCK, "capslock", 0 },
        { KEY_F1, "f1", 0 },
        { KEY_F2, "f2", 0 },
        { KEY_F3, "f3", 0 },
        { KEY_F4, "f4", 0 },
        { KEY_F5, "f5", 0 },
        { KEY_F6, "f6", 0 },
        { KEY_F7, "f7", 0 },
        { KEY_F8, "f8", 0 },
        { KEY_F9, "f9", 0 },
        { KEY_F10, "f10", 0 },
        { KEY_NUMLOCK, "numlock", 0 },
        { KEY_SCROLLLOCK, "scrolllock", 0 },
        { KEY_KP7, "kp7", 0 },
        { KEY_KP8, "kp8", 0 },
        { KEY_KP9, "kp9", 0 },
        { KEY_KPMINUS, "kpminus", 0 },
        { KEY_KP4, "kp4", 0 },
        { KEY_KP5, "kp5", 0 },
        { KEY_KP6, "kp6", 0 },
        { KEY_KPPLUS, "kpplus", 0 },
        { KEY_KP1, "kp1", 0 },
        { KEY_KP2, "kp2", 0 },
        { KEY_KP3, "kp3", 0 },
        { KEY_KP0, "kp0", 0 },
        { KEY_KPDOT, "kpdot", 0 },
        { KEY_ZENKAKUHANKAKU, "zenkakuhankaku", 0 },
        { KEY_102ND, "102nd", 0 },
        { KEY_F11, "f11", 0 },
        { KEY_F12, "f12", 0 },
        { KEY_RO, "ro", 0 },
        { KEY_KATAKANA, "katakana", 0 },
        { KEY_HIRAGANA, "hiragana", 0 },
        { KEY_HENKAN, "henkan", 0 },
        { KEY_KATAKANAHIRAGANA, "katakanahiragana", 0 },
        { KEY_MUHENKAN, "muhenkan", 0 },
        { KEY_KPJPCOMMA, "kpjpcomma", 0 },
        { KEY_KPENTER, "kpenter", 0 },
        { KEY_RIGHTCTRL, "rightctrl", 0 },
        { KEY_KPSLASH, "kpslash", 0 },
        { KEY_SYSRQ, "sysrq", 0 },
        { KEY_RIGHTALT, "rightalt", 0 },
        { KEY_LINEFEED, "linefeed", 0 },
        { KEY_HOME, "home", 0 },
        { KEY_UP, "up", 0 },
        { KEY_PAGEUP, "pageup", 0 },
        { KEY_LEFT, "left", 0 },
        { KEY_RIGHT, "right", 0 },
        { KEY_END, "end", 0 },
        { KEY_DOWN, "down", 0 },
        { KEY_PAGEDOWN, "pagedown", 0 },
        { KEY_INSERT, "insert", 0 },
        { KEY_DELETE, "delete", 0 },
        { KEY_MACRO, "macro", 0 },
        { KEY_MUTE, "mute", 0 },
        { KEY_VOLUMEDOWN, "volumedown", 0 },
        { KEY_VOLUMEUP, "volumeup", 0 },
        { KEY_POWER, "power", 0 },
        { KEY_KPEQUAL, "kpequal", 0 },
        { KEY_KPPLUSMINUS, "kpplusminus", 0 },
        { KEY_PAUSE, "pause", 0 },
        { KEY_SCALE, "scale", 0 },
        { KEY_KPCOMMA, "kpcomma", 0 },
        { KEY_HANGEUL, "hangeul", 0 },
        { KEY_HANGUEL, "hanguel", 0 },
        { KEY_HANJA, "hanja", 0 },
        { KEY_YEN, "yen", 0 },
        { KEY_LEFTMETA, "leftmeta", 0 },
        { KEY_RIGHTMETA, "rightmeta", 0 },
        { KEY_COMPOSE, "compose", 0 },
        { KEY_STOP, "stop", 0 },
        { KEY_AGAIN, "again", 0 },
        { KEY_PROPS, "props", 0 },
        { KEY_UNDO, "undo", 0 },
        { KEY_FRONT, "front", 0 },
        { KEY_COPY, "copy", 0 },
        { KEY_OPEN, "open", 0 },
        { KEY_PASTE, "paste", 0 },
        { KEY_FIND, "find", 0 },
        { KEY_CUT, "cut", 0 },
        { KEY_HELP, "help", 0 },
        { KEY_MENU, "menu", 0 },
        { KEY_CALC, "calc", 0 },
        { KEY_SETUP, "setup", 0 },
        { KEY_SLEEP, "sleep", 0 },
        { KEY_WAKEUP, "wakeup", 0 },
        { KEY_FILE, "file", 0 },
        { KEY_SENDFILE, "sendfile", 0 },
        { KEY_DELETEFILE, "deletefile", 0 },
        { KEY_XFER, "xfer", 0 },
        { KEY_PROG1, "prog1", 0 },
        { KEY_PROG2, "prog2", 0 },
        { KEY_WWW, "www", 0 },
        { KEY_MSDOS, "msdos", 0 },
        { KEY_COFFEE, "coffee", 0 },
        { KEY_SCREENLOCK, "screenlock", 0 },
        { KEY_DIRECTION, "direction", 0 },
        { KEY_CYCLEWINDOWS, "cyclewindows", 0 },
        { KEY_MAIL, "mail", 0 },
        { KEY_BOOKMARKS, "bookmarks", 0 },
        { KEY_COMPUTER, "computer", 0 },
        { KEY_BACK, "back", 0 },
        { KEY_FORWARD, "forward", 0 },
        { KEY_CLOSECD, "closecd", 0 },
        { KEY_EJECTCD, "ejectcd", 0 },
        { KEY_EJECTCLOSECD, "ejectclosecd", 0 },
        { KEY_NEXTSONG, "nextsong", 0 },
        { KEY_PLAYPAUSE, "playpause", 0 },
        { KEY_PREVIOUSSONG, "previoussong", 0 },
        { KEY_STOPCD, "stopcd", 0 },
        { KEY_RECORD, "record", 0 },
        { KEY_REWIND, "rewind", 0 },
        { KEY_PHONE, "phone", 0 },
        { KEY_ISO, "iso", 0 },
        { KEY_CONFIG, "config", 0 },
        { KEY_HOMEPAGE, "homepage", 0 },
        { KEY_REFRESH, "refresh", 0 },
        { KEY_EXIT, "exit", 0 },
        { KEY_MOVE, "move", 0 },
        { KEY_EDIT, "edit", 0 },
        { KEY_SCROLLUP, "scrollup", 0 },
        { KEY_SCROLLDOWN, "scrolldown", 0 },
        { KEY_KPLEFTPAREN, "kpleftparen", 0 },
        { KEY_KPRIGHTPAREN, "kprightparen", 0 },
        { KEY_NEW, "new", 0 },
        { KEY_REDO, "redo", 0 },
        { KEY_F13, "f13", 0 },
        { KEY_F14, "f14", 0 },
        { KEY_F15, "f15", 0 },
        { KEY_F16, "f16", 0 },
        { KEY_F17, "f17", 0 },
        { KEY_F18, "f18", 0 },
        { KEY_F19, "f19", 0 },
        { KEY_F20, "f20", 0 },
        { KEY_F21, "f21", 0 },
        { KEY_F22, "f22", 0 },
        { KEY_F23, "f23", 0 },
        { KEY_F24, "f24", 0 },
        { KEY_PLAYCD, "playcd", 0 },
        { KEY_PAUSECD, "pausecd", 0 },
        { KEY_PROG3, "prog3", 0 },
        { KEY_PROG4, "prog4", 0 },
        { KEY_DASHBOARD, "dashboard", 0 },
        { KEY_SUSPEND, "suspend", 0 },
        { KEY_CLOSE, "close", 0 },
        { KEY_PLAY, "play", 0 },
        { KEY_FASTFORWARD, "fastforward", 0 },
        { KEY_BASSBOOST, "bassboost", 0 },
        { KEY_PRINT, "print", 0 },
        { KEY_HP, "hp", 0 },
        { KEY_CAMERA, "camera", 0 },
        { KEY_SOUND, "sound", 0 },
        { KEY_QUESTION, "question", 0 },
        { KEY_EMAIL, "email", 0 },
        { KEY_CHAT, "chat", 0 },
        { KEY_SEARCH, "search", 0 },
        { KEY_CONNECT, "connect", 0 },
        { KEY_FINANCE, "finance", 0 },
        { KEY_SPORT, "sport", 0 },
        { KEY_SHOP, "shop", 0 },
        { KEY_ALTERASE, "alterase", 0 },
        { KEY_CANCEL, "cancel", 0 },
        { KEY_BRIGHTNESSDOWN, "brightnessdown", 0 },
        { KEY_BRIGHTNESSUP, "brightnessup", 0 },
        { KEY_MEDIA, "media", 0 },
        { KEY_SWITCHVIDEOMODE, "switchvideomode", 0 },
        { KEY_KBDILLUMTOGGLE, "kbdillumtoggle", 0 },
        { KEY_KBDILLUMDOWN, "kbdillumdown", 0 },
        { KEY_KBDILLUMUP, "kbdillumup", 0 },
        { KEY_SEND, "send", 0 },
        { KEY_REPLY, "reply", 0 },
        { KEY_FORWARDMAIL, "forwardmail", 0 },
        { KEY_SAVE, "save", 0 },
        { KEY_DOCUMENTS, "documents", 0 },
        { KEY_BATTERY, "battery", 0 },
        { KEY_BLUETOOTH, "bluetooth", 0 },
        { KEY_WLAN, "wlan", 0 },
        { KEY_UWB, "uwb", 0 },
        { KEY_UNKNOWN, "unknown", 0 },
        { KEY_VIDEO_NEXT, "video_next", 0 },
        { KEY_VIDEO_PREV, "video_prev", 0 },
        { KEY_BRIGHTNESS_CYCLE, "brightness_cycle", 0 },
        { KEY_BRIGHTNESS_ZERO, "brightness_zero", 0 },
        { KEY_DISPLAY_OFF, "display_off", 0 },
        { KEY_WIMAX, "wimax", 0 },
        { KEY_RFKILL, "rfkill", 0 },
        { KEY_MICMUTE, "micmute", 0 },
        { BTN_MISC, "misc", 0 },
        { BTN_0, "0", 0 },
        { BTN_1, "1", 0 },
        { BTN_2, "2", 0 },
        { BTN_3, "3", 0 },
        { BTN_4, "4", 0 },
        { BTN_5, "5", 0 },
        { BTN_6, "6", 0 },
        { BTN_7, "7", 0 },
        { BTN_8, "8", 0 },
        { BTN_9, "9", 0 },
        { BTN_MOUSE, "mouse", 0 },
        { BTN_LEFT, "left", 0 },
        { BTN_RIGHT, "right", 0 },
        { BTN_MIDDLE, "middle", 0 },
        { BTN_SIDE, "side", 0 },
        { BTN_EXTRA, "extra", 0 },
        { BTN_FORWARD, "forward", 0 },
        { BTN_BACK, "back", 0 },
        { BTN_TASK, "task", 0 },
        { BTN_JOYSTICK, "joystick", 0 },
        { BTN_TRIGGER, "trigger", 0 },
        { BTN_THUMB, "thumb", 0 },
        { BTN_THUMB2, "thumb2", 0 },
        { BTN_TOP, "top", 0 },
        { BTN_TOP2, "top2", 0 },
        { BTN_PINKIE, "pinkie", 0 },
        { BTN_BASE, "base", 0 },
        { BTN_BASE2, "base2", 0 },
        { BTN_BASE3, "base3", 0 },
        { BTN_BASE4, "base4", 0 },
        { BTN_BASE5, "base5", 0 },
        { BTN_BASE6, "base6", 0 },
        { BTN_DEAD, "dead", 0 },
        { BTN_GAMEPAD, "gamepad", 0 },
        { BTN_A, "a", 0 },
        { BTN_B, "b", 0 },
        { BTN_C, "c", 0 },
        { BTN_X, "x", 0 },
        { BTN_Y, "y", 0 },
        { BTN_Z, "z", 0 },
        { BTN_TL, "tl", 0 },
        { BTN_TR, "tr", 0 },
        { BTN_TL2, "tl2", 0 },
        { BTN_TR2, "tr2", 0 },
        { BTN_SELECT, "select", 0 },
        { BTN_START, "start", 0 },
        { BTN_MODE, "mode", 0 },
        { BTN_THUMBL, "thumbl", 0 },
        { BTN_THUMBR, "thumbr", 0 },
        { BTN_DIGI, "digi", 0 },
        { BTN_TOOL_PEN, "tool_pen", 0 },
        { BTN_TOOL_RUBBER, "tool_rubber", 0 },
        { BTN_TOOL_BRUSH, "tool_brush", 0 },
        { BTN_TOOL_PENCIL, "tool_pencil", 0 },
        { BTN_TOOL_AIRBRUSH, "tool_airbrush", 0 },
        { BTN_TOOL_FINGER, "tool_finger", 0 },
        { BTN_TOOL_MOUSE, "tool_mouse", 0 },
        { BTN_TOOL_LENS, "tool_lens", 0 },
        { BTN_TOOL_QUINTTAP, "tool_quinttap", 0 },
        { BTN_TOUCH, "touch", 0 },
        { BTN_STYLUS, "stylus", 0 },
        { BTN_STYLUS2, "stylus2", 0 },
        { BTN_TOOL_DOUBLETAP, "tool_doubletap", 0 },
        { BTN_TOOL_TRIPLETAP, "tool_tripletap", 0 },
        { BTN_TOOL_QUADTAP, "tool_quadtap", 0 },
        { BTN_WHEEL, "wheel", 0 },
        { BTN_GEAR_DOWN, "gear_down", 0 },
        { BTN_GEAR_UP, "gear_up", 0 },
        { KEY_OK, "ok", 0 },
        { KEY_SELECT, "select", 0 },
        { KEY_GOTO, "goto", 0 },
        { KEY_CLEAR, "clear", 0 },
        { KEY_POWER2, "power2", 0 },
        { KEY_OPTION, "option", 0 },
        { KEY_INFO, "info", 0 },
        { KEY_TIME, "time", 0 },
        { KEY_VENDOR, "vendor", 0 },
        { KEY_ARCHIVE, "archive", 0 },
        { KEY_PROGRAM, "program", 0 },
        { KEY_CHANNEL, "channel", 0 },
        { KEY_FAVORITES, "favorites", 0 },
        { KEY_EPG, "epg", 0 },
        { KEY_PVR, "pvr", 0 },
        { KEY_MHP, "mhp", 0 },
        { KEY_LANGUAGE, "language", 0 },
        { KEY_TITLE, "title", 0 },
        { KEY_SUBTITLE, "subtitle", 0 },
        { KEY_ANGLE, "angle", 0 },
        { KEY_ZOOM, "zoom", 0 },
        { KEY_MODE, "mode", 0 },
        { KEY_KEYBOARD, "keyboard", 0 },
        { KEY_SCREEN, "screen", 0 },
        { KEY_PC, "pc", 0 },
        { KEY_TV, "tv", 0 },
        { KEY_TV2, "tv2", 0 },
        { KEY_VCR, "vcr", 0 },
        { KEY_VCR2, "vcr2", 0 },
        { KEY_SAT, "sat", 0 },
        { KEY_SAT2, "sat2", 0 },
        { KEY_CD, "cd", 0 },
        { KEY_TAPE, "tape", 0 },
        { KEY_RADIO, "radio", 0 },
        { KEY_TUNER, "tuner", 0 },
        { KEY_PLAYER, "player", 0 },
        { KEY_TEXT, "text", 0 },
        { KEY_DVD, "dvd", 0 },
        { KEY_AUX, "aux", 0 },
        { KEY_MP3, "mp3", 0 },
        { KEY_AUDIO, "audio", 0 },
        { KEY_VIDEO, "video", 0 },
        { KEY_DIRECTORY, "directory", 0 },
        { KEY_LIST, "list", 0 },
        { KEY_MEMO, "memo", 0 },
        { KEY_CALENDAR, "calendar", 0 },
        { KEY_RED, "red", 0 },
        { KEY_GREEN, "green", 0 },
        { KEY_YELLOW, "yellow", 0 },
        { KEY_BLUE, "blue", 0 },
        { KEY_CHANNELUP, "channelup", 0 },
        { KEY_CHANNELDOWN, "channeldown", 0 },
        { KEY_FIRST, "first", 0 },
        { KEY_LAST, "last", 0 },
        { KEY_AB, "ab", 0 },
        { KEY_NEXT, "next", 0 },
        { KEY_RESTART, "restart", 0 },
        { KEY_SLOW, "slow", 0 },
        { KEY_SHUFFLE, "shuffle", 0 },
        { KEY_BREAK, "break", 0 },
        { KEY_PREVIOUS, "previous", 0 },
        { KEY_DIGITS, "digits", 0 },
        { KEY_TEEN, "teen", 0 },
        { KEY_TWEN, "twen", 0 },
        { KEY_VIDEOPHONE, "videophone", 0 },
        { KEY_GAMES, "games", 0 },
        { KEY_ZOOMIN, "zoomin", 0 },
        { KEY_ZOOMOUT, "zoomout", 0 },
        { KEY_ZOOMRESET, "zoomreset", 0 },
        { KEY_WORDPROCESSOR, "wordprocessor", 0 },
        { KEY_EDITOR, "editor", 0 },
        { KEY_SPREADSHEET, "spreadsheet", 0 },
        { KEY_GRAPHICSEDITOR, "graphicseditor", 0 },
        { KEY_PRESENTATION, "presentation", 0 },
        { KEY_DATABASE, "database", 0 },
        { KEY_NEWS, "news", 0 },
        { KEY_VOICEMAIL, "voicemail", 0 },
        { KEY_ADDRESSBOOK, "addressbook", 0 },
        { KEY_MESSENGER, "messenger", 0 },
        { KEY_DISPLAYTOGGLE, "displaytoggle", 0 },
        { KEY_SPELLCHECK, "spellcheck", 0 },
        { KEY_LOGOFF, "logoff", 0 },
        { KEY_DOLLAR, "dollar", 0 },
        { KEY_EURO, "euro", 0 },
        { KEY_FRAMEBACK, "frameback", 0 },
        { KEY_FRAMEFORWARD, "frameforward", 0 },
        { KEY_CONTEXT_MENU, "context_menu", 0 },
        { KEY_MEDIA_REPEAT, "media_repeat", 0 },
        { KEY_10CHANNELSUP, "10channelsup", 0 },
        { KEY_10CHANNELSDOWN, "10channelsdown", 0 },
        { KEY_IMAGES, "images", 0 },
        { KEY_DEL_EOL, "del_eol", 0 },
        { KEY_DEL_EOS, "del_eos", 0 },
        { KEY_INS_LINE, "ins_line", 0 },
        { KEY_DEL_LINE, "del_line", 0 },
        { KEY_FN, "fn", 0 },
        { KEY_FN_ESC, "fn_esc", 0 },
        { KEY_FN_F1, "fn_f1", 0 },
        { KEY_FN_F2, "fn_f2", 0 },
        { KEY_FN_F3, "fn_f3", 0 },
        { KEY_FN_F4, "fn_f4", 0 },
        { KEY_FN_F5, "fn_f5", 0 },
        { KEY_FN_F6, "fn_f6", 0 },
        { KEY_FN_F7, "fn_f7", 0 },
        { KEY_FN_F8, "fn_f8", 0 },
        { KEY_FN_F9, "fn_f9", 0 },
        { KEY_FN_F10, "fn_f10", 0 },
        { KEY_FN_F11, "fn_f11", 0 },
        { KEY_FN_F12, "fn_f12", 0 },
        { KEY_FN_1, "fn_1", 0 },
        { KEY_FN_2, "fn_2", 0 },
        { KEY_FN_D, "fn_d", 0 },
        { KEY_FN_E, "fn_e", 0 },
        { KEY_FN_F, "fn_f", 0 },
        { KEY_FN_S, "fn_s", 0 },
        { KEY_FN_B, "fn_b", 0 },
        { KEY_BRL_DOT1, "brl_dot1", 0 },
        { KEY_BRL_DOT2, "brl_dot2", 0 },
        { KEY_BRL_DOT3, "brl_dot3", 0 },
        { KEY_BRL_DOT4, "brl_dot4", 0 },
        { KEY_BRL_DOT5, "brl_dot5", 0 },
        { KEY_BRL_DOT6, "brl_dot6", 0 },
        { KEY_BRL_DOT7, "brl_dot7", 0 },
        { KEY_BRL_DOT8, "brl_dot8", 0 },
        { KEY_BRL_DOT9, "brl_dot9", 0 },
        { KEY_BRL_DOT10, "brl_dot10", 0 },
        { KEY_NUMERIC_0, "numeric_0", 0 },
        { KEY_NUMERIC_1, "numeric_1", 0 },
        { KEY_NUMERIC_2, "numeric_2", 0 },
        { KEY_NUMERIC_3, "numeric_3", 0 },
        { KEY_NUMERIC_4, "numeric_4", 0 },
        { KEY_NUMERIC_5, "numeric_5", 0 },
        { KEY_NUMERIC_6, "numeric_6", 0 },
        { KEY_NUMERIC_7, "numeric_7", 0 },
        { KEY_NUMERIC_8, "numeric_8", 0 },
        { KEY_NUMERIC_9, "numeric_9", 0 },
        { KEY_NUMERIC_STAR, "numeric_star", 0 },
        { KEY_NUMERIC_POUND, "numeric_pound", 0 },
        { KEY_CAMERA_FOCUS, "camera_focus", 0 },
        { KEY_WPS_BUTTON, "wps_button", 0 },
        { KEY_TOUCHPAD_TOGGLE, "touchpad_toggle", 0 },
        { KEY_TOUCHPAD_ON, "touchpad_on", 0 },
        { KEY_TOUCHPAD_OFF, "touchpad_off", 0 },
        { KEY_CAMERA_ZOOMIN, "camera_zoomin", 0 },
        { KEY_CAMERA_ZOOMOUT, "camera_zoomout", 0 },
        { KEY_CAMERA_UP, "camera_up", 0 },
        { KEY_CAMERA_DOWN, "camera_down", 0 },
        { KEY_CAMERA_LEFT, "camera_left", 0 },
        { KEY_CAMERA_RIGHT, "camera_right", 0 },
        { BTN_TRIGGER_HAPPY, "trigger_happy", 0 },
        { BTN_TRIGGER_HAPPY1, "trigger_happy1", 0 },
        { BTN_TRIGGER_HAPPY2, "trigger_happy2", 0 },
        { BTN_TRIGGER_HAPPY3, "trigger_happy3", 0 },
        { BTN_TRIGGER_HAPPY4, "trigger_happy4", 0 },
        { BTN_TRIGGER_HAPPY5, "trigger_happy5", 0 },
        { BTN_TRIGGER_HAPPY6, "trigger_happy6", 0 },
        { BTN_TRIGGER_HAPPY7, "trigger_happy7", 0 },
        { BTN_TRIGGER_HAPPY8, "trigger_happy8", 0 },
        { BTN_TRIGGER_HAPPY9, "trigger_happy9", 0 },
        { BTN_TRIGGER_HAPPY10, "trigger_happy10", 0 },
        { BTN_TRIGGER_HAPPY11, "trigger_happy11", 0 },
        { BTN_TRIGGER_HAPPY12, "trigger_happy12", 0 },
        { BTN_TRIGGER_HAPPY13, "trigger_happy13", 0 },
        { BTN_TRIGGER_HAPPY14, "trigger_happy14", 0 },
        { BTN_TRIGGER_HAPPY15, "trigger_happy15", 0 },
        { BTN_TRIGGER_HAPPY16, "trigger_happy16", 0 },
        { BTN_TRIGGER_HAPPY17, "trigger_happy17", 0 },
        { BTN_TRIGGER_HAPPY18, "trigger_happy18", 0 },
        { BTN_TRIGGER_HAPPY19, "trigger_happy19", 0 },
        { BTN_TRIGGER_HAPPY20, "trigger_happy20", 0 },
        { BTN_TRIGGER_HAPPY21, "trigger_happy21", 0 },
        { BTN_TRIGGER_HAPPY22, "trigger_happy22", 0 },
        { BTN_TRIGGER_HAPPY23, "trigger_happy23", 0 },
        { BTN_TRIGGER_HAPPY24, "trigger_happy24", 0 },
        { BTN_TRIGGER_HAPPY25, "trigger_happy25", 0 },
        { BTN_TRIGGER_HAPPY26, "trigger_happy26", 0 },
        { BTN_TRIGGER_HAPPY27, "trigger_happy27", 0 },
        { BTN_TRIGGER_HAPPY28, "trigger_happy28", 0 },
        { BTN_TRIGGER_HAPPY29, "trigger_happy29", 0 },
        { BTN_TRIGGER_HAPPY30, "trigger_happy30", 0 },
        { BTN_TRIGGER_HAPPY31, "trigger_happy31", 0 },
        { BTN_TRIGGER_HAPPY32, "trigger_happy32", 0 },
        { BTN_TRIGGER_HAPPY33, "trigger_happy33", 0 },
        { BTN_TRIGGER_HAPPY34, "trigger_happy34", 0 },
        { BTN_TRIGGER_HAPPY35, "trigger_happy35", 0 },
        { BTN_TRIGGER_HAPPY36, "trigger_happy36", 0 },
        { BTN_TRIGGER_HAPPY37, "trigger_happy37", 0 },
        { BTN_TRIGGER_HAPPY38, "trigger_happy38", 0 },
        { BTN_TRIGGER_HAPPY39, "trigger_happy39", 0 },
        { BTN_TRIGGER_HAPPY40, "trigger_happy40", 0 }
    };

    static char first_call = 1;
    int i = 0;

    // If this is the first call, initialize relative caps
    if (first_call) {
        for(i = 0; i < sizeof(cap_map) / sizeof(cap_map[0]); ++i)
            cap_map[i].atom = driver_mk_atom(cap_map[i].name);
        first_call = 0;
    }

    if (ioctl(fd, EVIOCGBIT(EV_KEY, sizeof(key_bitmask)), key_bitmask) < 0) {
        printf("ioctl EVIOCGBIT failed: %s\n", strerror(errno));
        return IEDRV_RES_IO_ERROR;
    }

    dterm_tuple_begin(dt, &prop);
    dterm_atom(dt, ie_key);
    dterm_list_begin(dt, &c_list);

    // Add all capabilities present
    for(i = 0; i < sizeof(cap_map) / sizeof(cap_map[0]); ++i)
        if (t_bit(key_bitmask, cap_map[i].bit))
            dterm_atom(dt, cap_map[i].atom);

    dterm_list_end(dt, &c_list);
    dterm_tuple_end(dt, &prop);

    return IEDRV_RES_OK;
}

static unsigned char add_rel_cap(dterm_t* dt, int fd)
{
    dterm_mark_t prop;
    dterm_mark_t c_list;
    unsigned char rel_bitmask[REL_CNT / 8 + 1];
    static struct {
        unsigned int bit;
        char* name;
        ErlDrvTermData atom;
    } cap_map[] = {
        { REL_X, "x", 0 },
        { REL_Y, "y", 0 },
        { REL_Z, "z", 0 },
        { REL_RX, "rx", 0 },
        { REL_RY, "rx", 0 },
        { REL_RZ, "rx", 0 },
        { REL_HWHEEL, "hwheel", 0 },
        { REL_DIAL, "dial", 0 },
        { REL_WHEEL, "wheel", 0 },
        { REL_MISC, "misc", 0 }
    };
    static char first_call = 1;
    int i = 0;

    // If this is the first call, initialize relative caps
    if (first_call) {
        for(i = 0; i < sizeof(cap_map) / sizeof(cap_map[0]); ++i)
            cap_map[i].atom = driver_mk_atom(cap_map[i].name);
        first_call = 0;
    }

    if (ioctl(fd, EVIOCGBIT(EV_REL, sizeof(rel_bitmask)), rel_bitmask) < 0) {
        printf("ioctl EVIOCGBIT failed: %s\n", strerror(errno));
        return IEDRV_RES_IO_ERROR;
    }

    dterm_tuple_begin(dt, &prop);
    dterm_atom(dt, ie_relative);
    dterm_list_begin(dt, &c_list);

    // Add all capabilities present
    for(i = 0; i < sizeof(cap_map) / sizeof(cap_map[0]); ++i)
        if (t_bit(rel_bitmask, cap_map[i].bit))
            dterm_atom(dt, cap_map[i].atom);

    dterm_list_end(dt, &c_list);
    dterm_tuple_end(dt, &prop);

    return IEDRV_RES_OK;
}

static unsigned char add_sync_cap(dterm_t* dt, int fd)
{
    dterm_mark_t prop;
    dterm_mark_t c_list;

    dterm_tuple_begin(dt, &prop);
    dterm_atom(dt, ie_sync);
    dterm_list_begin(dt, &c_list);
    dterm_list_end(dt, &c_list);
    dterm_tuple_end(dt, &prop);

    return IEDRV_RES_OK;
}
