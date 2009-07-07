AIX_P = exportlist.aix

exportlist.aix: $(OBJS)
	$(RM) exportlist.aix
	for f in $(OBJS); do \
	  /usr/ccs/bin/nm -B -e $$f | grep ' T [^ ][^ ]*$$' | \
	  sed -e 's/^.* T \.*\([^ ][^ ]*\)$$/\1/' >> exportlist.aix; \
        done;
