all:
	rebar \
		x-comp-erlang-root=../../erl-arm-none-linux-gnueabi \
		x-comp-toolchain-root=/opt/arm-2007q1 \
		x-comp-target-arch=arm-none-linux-gnueabi \
	compile

clean:
	rebar clean
