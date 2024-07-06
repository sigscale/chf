## Makefile.am ##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = chf.app
EXTRA_DIST = chf.app

beam_DATA = chf.beam \
		chf_app.beam \
		chf_sup.beam \
		chf_nchf_sup.beam \
		chf_nchf_listener_sup.beam \
		chf_nrf_sup.beam \
		chf_nrf_connection_fsm.beam \
		chf_server.beam \
		chf_nchf_handler.beam \
		chf_rest.beam

%.beam:	$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			< $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

