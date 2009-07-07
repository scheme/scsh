/* Exports from flock1.c. */

s48_value set_lock(s48_value fd, s48_value cmd, s48_value type, 
		   s48_value whence, s48_value start, s48_value len);
s48_value get_lock(s48_value fd, s48_value cmd, s48_value type, 
		   s48_value whence, s48_value start, s48_value len);
void s48_init_flock(void);
