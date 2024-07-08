## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

check_DATA = .dialyzer_ok .common_test_ok

testmods = chf_ct_lib \
		chf_rf_server \
		chf_nrf_handler \
		chf_api_SUITE \
		chf_nchf_SUITE

DIA_ARGS =
CT_ARGS = -spec test.spec

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer $(DIA_ARGS) --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl
	$(ERLC) $(ERLCFLAGS) $<

.common_test_ok:	$(addsuffix .beam, $(testmods))
	@if [ ! -d chf_nchf_SUITE_data ]; then mkdir chf_nchf_SUITE_data; fi
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	(cd chf_nchf_SUITE_data && \
		if [ ! -f CAkey.pem -o ! -f CAcert.pem ]; then \
			openssl req -newkey rsa:2048 -nodes -x509 -days 30 \
					-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=ca.$$(hostname)\/emailAddress=support@$$(hostname) \
					-keyout CAkey.pem -out CAcert.pem; \
		fi; \
		if [ ! -f key.pem -o ! -f cert.csr ]; then \
		openssl req -newkey rsa:2048 -nodes \
				-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
				-keyout key.pem -out cert.csr; \
				chmod 600 key.pem; \
		fi; \
		if [ ! -f extensions ]; then \
			echo "extendedKeyUsage = serverAuth" > extensions; \
			echo "subjectAltName = DNS:$$(hostname)" >> extensions; \
		fi; \
		if [ ! -f cert.pem ]; then \
			openssl x509 -req -CA CAcert.pem -CAkey CAkey.pem -CAcreateserial \
					-extfile extensions -in cert.csr -out cert.pem; \
		fi)
	ct_run -no_auto_compile \
			-pa $(top_builddir)/ebin \
			$(CT_ARGS) \
			-logdir log \
			-config ct.cfg \
			-dir . \
			-erl_args -noshell \
			-sname $(LOGNAME)-ct

clean-local:
	-rm -r *.beam log/*

