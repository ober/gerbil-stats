ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)"
default: linux-static

build-release:
	git config --global --add safe.directory /src
	/opt/gerbil/bin/gxpkg deps -i
	/opt/gerbil/bin/gxpkg build --release

linux-static:
	docker run -t \
	-e UID=$(id -u) \
	-e GID=$(id -g) \
	-e USER=$(USER) \
	-e GERBIL_PATH=/src/.gerbil \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src/ build-release

install:
	mv .gerbil/bin/stats /usr/local/bin/stats

clean:
	gerbil clean
	gerbil clean all
