/* Exports from network1.c. */

s48_value scsh_socket (s48_value sch_pf, s48_value sch_type, 
		       s48_value sch_protocol);

s48_value scheme_bind(s48_value sockfd, s48_value family, s48_value scheme_name);

s48_value scheme_connect(s48_value sock, s48_value family, 
			 s48_value scheme_name);

s48_value scsh_listen (s48_value sch_sockfd, s48_value sch_backlog);

s48_value scheme_accept(s48_value sockfd, s48_value family);

s48_value scheme_peer_name(s48_value sockfd, s48_value family);

s48_value scsh_shutdown (s48_value sch_sockfd, s48_value sch_how);

s48_value scheme_socket_name(s48_value sockfd, s48_value family);

s48_value scheme_socket_pair(s48_value type);

s48_value recv_substring(s48_value s, s48_value flags, s48_value buf, 
			 s48_value start, s48_value end);

s48_value send_substring(s48_value s, s48_value flags, s48_value buf, 
			 s48_value start, s48_value end, s48_value family, 
			 s48_value scheme_name);

s48_value scheme_getsockopt (s48_value s, s48_value level, s48_value optname);

s48_value scheme_getsockopt_linger (s48_value s,
				    s48_value level,
				    s48_value optname);
				    

s48_value scheme_getsockopt_timeout (s48_value s,
				     s48_value level,
				     s48_value optname);


s48_value scheme_setsockopt (s48_value s,
			     s48_value level,
			     s48_value optname,
			     s48_value optval);

s48_value scheme_setsockopt_linger (s48_value s,
				    s48_value level,
				    s48_value optname,
				    s48_value onoff,
				    s48_value linger);

s48_value scheme_setsockopt_timeout (s48_value s,
				     s48_value level,
				     s48_value optname,
				     s48_value sec,
				     s48_value usec);

s48_value scheme_host_address2host_info(s48_value scheme_byte_vector);

s48_value scheme_host_name2host_info(s48_value scheme_name);

s48_value scheme_net_address2net_info(s48_value net_addr);

s48_value scheme_net_name2net_info(s48_value scheme_name);

s48_value scheme_serv_port2serv_info(s48_value in_port,
				     s48_value in_proto);

s48_value scheme_serv_name2serv_info(s48_value in_name,
				     s48_value in_proto);

s48_value scheme_proto_num2proto_info(s48_value in_proto);

s48_value scheme_proto_name2proto_info(s48_value name);

void s48_init_network(void);
