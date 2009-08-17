/* Exports from fdports1.c. */

s48_value read_delim(const char *delims, char *buf,
			int fd, int start, int end,
			int *nread);

s48_value skip_chars(const char *skipchars, int fd,  int *nread);
