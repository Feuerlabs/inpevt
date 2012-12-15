PRIVDIR := ../priv

all : $(PRIVDIR)/inpevt_driver.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/inpevt_driver.so : inpevt_driver.o dterm.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/inpevt_driver.so inpevt_driver.o dterm.o
