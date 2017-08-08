src_dir = src

.PHONY: all doc install clean

all:
	$(MAKE) -C $(src_dir)

install:
	$(MAKE) install -C $(src_dir)

clean:
	$(MAKE) clean -C $(src_dir)
