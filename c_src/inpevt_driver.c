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

#define DEBUG 1

static ErlDrvData inpevt_start (ErlDrvPort port, char *command);

static void inpevt_stop (ErlDrvData drv_data);

static ErlDrvSSizeT inpevt_control(ErlDrvData drv_data,
                                   unsigned int command,
                                   char *buf,
                                   ErlDrvSizeT len,
                                   char **rbuf,
                                   ErlDrvSizeT rlen);

static void inpevt_ready_input(ErlDrvData drv_data, ErlDrvEvent event);

#define GPIO_DRV_MAJOR_VER 1
#define GPIO_DRV_MINOR_VER 0

static ErlDrvEntry inpevt_driver_entry = {
    NULL,                        // init
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

#define GPIODRV_CMD_MASK  0x0000000F

#define GPIODRV_CMD_OPEN  0x00000001
#define GPIODRV_CMD_CLOSE 0x00000002
#define GPIODRV_CMD_ACTIVATE 0x00000003
#define GPIODRV_CMD_DEACTIVATE 0x00000004

#define GPIODRV_CMD_GET_BUS_TYPE 0x00000003

#define GPIODRV_CMD_ARG_MASK 0x000000F0
#define GPIODRV_CMD_ARG_LOW 0x00000010
#define GPIODRV_CMD_ARG_HIGH 0x00000020

#define GPIODRV_RES_OK 0
#define GPIODRV_RES_IO_ERROR 1
#define GPIODRV_RES_NOT_OPEN 2
#define GPIODRV_RES_ILLEGAL_ARG 3


typedef struct {
    ErlDrvPort mPort;
    int mDescriptor; // Opened mDevice
    char mDevice[256]; // /dev/input/...
    char mActive;  // Are events reported or not?
} GPIOContext;

DRIVER_INIT(inpevt_driver)
{
    return &inpevt_driver_entry;
}


static ErlDrvData inpevt_start(ErlDrvPort port, char *command)
{
    GPIOContext *ctx = 0;

    ctx = (GPIOContext*) driver_alloc(sizeof(GPIOContext));
    ctx->mDescriptor = -1;
    ctx->mPort = port;
    ctx->mActive = 0;
    ctx->mDevice[0] = 0;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData) ctx;
}

static void inpevt_stop (ErlDrvData drv_data)
{
    driver_free(drv_data);
}


static ErlDrvSSizeT inpevt_open_port(GPIOContext* ctx)
{
    if (ctx->mDescriptor != -1) {
        close(ctx->mDescriptor);
        ctx->mDescriptor = -1;
    }

    ctx->mDescriptor = open(ctx->mDevice, O_RDONLY);
    if (ctx->mDescriptor == -1) {
        printf("Failed to open %s: %s\n\r", ctx->mDevice, strerror(errno));
        return GPIODRV_RES_IO_ERROR;
    }
    return GPIODRV_RES_OK;
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
    GPIOContext* ctx = 0;

    ctx = (GPIOContext*) drv_data;

    switch(command & GPIODRV_CMD_MASK) {
    case GPIODRV_CMD_OPEN:
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

        **rbuf =  inpevt_open_port(ctx);

        return 1;
    }

    case GPIODRV_CMD_ACTIVATE:
        if (ctx->mDescriptor == -1) {
            **rbuf = GPIODRV_RES_NOT_OPEN;
            return 1;
        }
        ctx->mActive = 1;
        driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 1);
        **rbuf = GPIODRV_RES_OK;
        return 1;


    case GPIODRV_CMD_DEACTIVATE:
        if (ctx->mDescriptor == -1) {
            **rbuf = GPIODRV_RES_NOT_OPEN;
            return 1;
        }
        if (ctx->mActive == 1) {
            ctx->mActive = 0;
            driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 0);
        }
        **rbuf = GPIODRV_RES_OK;
        return 1;


    // Are we closing?
    case GPIODRV_CMD_CLOSE:
        // Remove from select set.
        if (ctx->mDescriptor != -1 && ctx->mActive == 1)
            driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 0);

        close(ctx->mDescriptor);

        // FIXME: UNEXPORT
        ctx->mDescriptor = -1;
        ctx->mActive = 0;

        **rbuf = GPIODRV_RES_OK;
        return 1;

    default:
        break;
    }

    puts("Illegal command\r");

    **rbuf = GPIODRV_RES_ILLEGAL_ARG;
    return 1;
}


static void inpevt_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    GPIOContext* ctx = 0;
    char buf[256];
    ssize_t rd_res = 0;

    ctx = (GPIOContext*) drv_data;

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


