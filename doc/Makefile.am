## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = chf.html \
		chf_app.html \
		chf_sup.html \
		chf_nchf_sup.html \
		chf_nchf_listener_sup.html \
		chf_nrf_sup.html \
		chf_nrf_connection_sup.html \
		chf_nrf_connection_fsm.html \
		chf_server.html \
		chf_nchf_handler.html \
		chf_rest.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html \
		overview-summary.html \
		index.html \
		stylesheet.css \
		edoc-info \
		sigscale-logo.png \
		supervision.svg \
		interfaces.svg

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/supervision.svg .
	-cp -n $(top_srcdir)/doc/interfaces.svg .

CLEANFILES = $(doc_DATA) $(modhtml) erlang.png

