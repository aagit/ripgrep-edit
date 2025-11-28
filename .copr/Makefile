spec_template   ?= .distro/ripgrep-edit.spec.template
spec		:= dist/ripgrep-edit.spec

PKG_VERSION	:= $(shell grep '^version = ' Cargo.toml | sed 's,.*\"\(.*\)\",\1,')
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
	sed -e "s,@@RIPGREP-VERSION@@,$(PKG_VERSION)," $(spec_template) > $(spec)
	rpmbuild -bs --define "_sourcedir dist" --define "_srcrpmdir $(outdir)" $(spec)

clean:
	rm -rf $(PKG_TARBALL) dist rpm
