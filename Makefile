PLATFORMS := linux/amd64/ windows/amd64/.exe /linux/386/32 /windows/386/32.exe

temp = $(subst /, ,$@)
os = $(word 1, $(temp))
arch = $(word 2, $(temp))
suffix = $(word 3, $(temp))

release: $(PLATFORMS)

clean:
	go clean
	rm -f vigilantbsp
	rm -f vigilantbsp.exe
	rm -f vigilantbsp32
	rm -f vigilantbsp32.exe

$(PLATFORMS):
	GOOS=$(os) GOARCH=$(arch) go build -ldflags="-s -w -buildid=" -trimpath -o 'vigilantbsp$(suffix)'

