spec            ?= .distro/ripgrep-edit.spec

PKG_VERSION	:= $(shell rpmspec --query --queryformat '%{VERSION}' $(spec))
PKG_TARBALL	:= dist/ripgrep-edit-$(PKG_VERSION).tar.gz

default:
	@echo "targets: dist srpm clean"

.PHONY: dist
dist tarball $(PKG_TARBALL):
	rm -rf dist
	mkdir dist
	tar -czf $(PKG_TARBALL) --exclude ".git" --exclude "dist" --exclude ".distro" --exclude "release/*" --transform "s,^./,ripgrep-edit-$(PKG_VERSION)/," .

srpm: $(PKG_TARBALL)
	rm -rf rpm
	mkdir rpm
	rpmbuild -bs --define "_sourcedir dist" --define "_srcrpmdir $(outdir)" $(spec)

clean:
	rm -rf $(PKG_TARBALL) dist rpm
