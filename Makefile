all:
	X_COMP_OTP_ROOT=../../erl-arm-none-linux-gnueabi \
	X_COMP_TOOLCHAIN_ROOT=/opt/arm-2007q1 \
	X_COMP_TARGET_ARCH=arm-none-linux-gnueabi \
	rebar -v3 compile

clean:
	rebar clean
