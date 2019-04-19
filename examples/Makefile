all:
	mkdir -p gh-pages
	cp index.html gh-pages/

	$(MAKE) -C listbox
	mkdir -p gh-pages/listbox
	cp listbox/index.html gh-pages/listbox/
	cp listbox/elm.js gh-pages/listbox/

	$(MAKE) -C listbox-combobox
	mkdir -p gh-pages/listbox-combobox
	cp listbox-combobox/index.html gh-pages/listbox-combobox/
	cp listbox-combobox/elm.js gh-pages/listbox-combobox/

	$(MAKE) -C listbox-dropdown
	mkdir -p gh-pages/listbox-dropdown
	cp listbox-dropdown/index.html gh-pages/listbox-dropdown/
	cp listbox-dropdown/elm.js gh-pages/listbox-dropdown/

	$(MAKE) -C listbox-unique
	mkdir -p gh-pages/listbox-unique
	cp listbox-unique/index.html gh-pages/listbox-unique/
	cp listbox-unique/elm.js gh-pages/listbox-unique/
