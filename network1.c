/* 
 * Scheme48/scsh network interface.
 * Routines that require custom C support.
 * Copyright (c) 1994 by Brian D. Carlstrom
 * Copyright (c) 1994 by Olin Shivers
 */
#include "sysdep.h"
#include "cstuff.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
/* Make sure our exports match up w/the implementation: */
#include "network1.h"
#include "scheme48.h"

s48_value scsh_socket (s48_value sch_pf, s48_value sch_type, 
		       s48_value sch_protocol)
{
  int retval = socket (s48_extract_integer (sch_pf),
		       s48_extract_integer (sch_type),
		       s48_extract_integer (sch_protocol));
  if (retval == -1)
    s48_raise_os_error_3 (errno, sch_pf, sch_type, sch_protocol);
  return s48_enter_fixnum (retval);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_bind(s48_value sock, s48_value family, s48_value scheme_name)
{
  int sockfd = s48_extract_fixnum (sock);
  switch(s48_extract_fixnum (family))
    {
    case AF_UNIX: 
      {
	struct sockaddr_un name;
	int scheme_length=S48_STRING_LENGTH(scheme_name);
	memset(&name, 0, sizeof(name));

	name.sun_family=AF_UNIX;	

	strncpy(name.sun_path,
		s48_extract_string(scheme_name),
		scheme_length);	/* copy to c string */
	name.sun_path[scheme_length]='\0'; /* add null */
	if ( bind(sockfd,(struct sockaddr *)&name,sizeof(name)) < 0)
	  s48_raise_os_error_3 (errno, sock, family, scheme_name);
	return S48_UNSPECIFIC;
      }
    case AF_INET: 
      {
	struct sockaddr_in name;
	unsigned long  addr = htonl(s48_extract_unsigned_integer (S48_CAR (scheme_name)));
	unsigned short port = htons(s48_extract_fixnum (S48_CDR (scheme_name)));
	memset(&name, 0, sizeof(name));

	name.sin_family=AF_INET;
	name.sin_addr.s_addr=addr;
	name.sin_port=port;
	if (bind(sockfd,(struct sockaddr *)&name,sizeof(name)) < 0)
	  s48_raise_os_error_3(errno, sock, family, scheme_name);

	return S48_UNSPECIFIC;
      }
    default:
	s48_raise_argument_type_error (family);	
    }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_connect(s48_value sock, s48_value family, s48_value scheme_name)
{
  int sockfd = s48_extract_fixnum (sock);
  switch(s48_extract_fixnum (family))
    {
    case AF_UNIX: 
      {
	struct sockaddr_un name;
	int scheme_length=S48_STRING_LENGTH(scheme_name);
	
	memset(&name, 0, sizeof(name));
	
	name.sun_family=AF_UNIX;	

	strncpy(name.sun_path,
		s48_extract_string (scheme_name),
		scheme_length);	/* copy to c string */
	name.sun_path[scheme_length]='\0'; /* add null */
	
	if (connect(sockfd,(struct sockaddr *)&name,sizeof(name)) == 0)
	  return s48_cons (S48_FALSE, S48_UNSPECIFIC);
	
	if (errno != EWOULDBLOCK && errno != EINTR && errno != EALREADY
	    && errno != EINPROGRESS && errno != EAGAIN)
	  s48_raise_os_error_3(errno, sock, family, scheme_name);
	 
	return s48_cons (S48_TRUE, (errno == EINPROGRESS) ? S48_TRUE : S48_FALSE);	
      }
    case AF_INET: 
      {
	struct sockaddr_in name;
	
	u_long addr= htonl(s48_extract_unsigned_integer (S48_CAR (scheme_name)));
	u_short port= htons(s48_extract_fixnum (S48_CDR (scheme_name)));

	memset(&name, 0, sizeof(name));
	
	name.sin_family=AF_INET;
	name.sin_addr.s_addr=addr;
	name.sin_port=port;
	
	if (connect(sockfd,(struct sockaddr *)&name,sizeof(name)) == 0)
	  return s48_cons (S48_FALSE, S48_UNSPECIFIC);
	
	if (errno != EWOULDBLOCK && errno != EINTR && errno != EALREADY
	    && errno != EINPROGRESS && errno != EAGAIN)
	  s48_raise_os_error_3(errno, sock, family, scheme_name);
	
	return s48_cons (S48_TRUE, (errno == EINPROGRESS) ? S48_TRUE : S48_FALSE);
      }
    default: 
      s48_raise_argument_type_error (family);	/* error unknown address family */ 
    } 
}
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scsh_listen (s48_value sch_sockfd, s48_value sch_backlog)
{
  int retval = listen (s48_extract_fixnum (sch_sockfd),
		       s48_extract_integer (sch_backlog));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_sockfd, sch_backlog);
  return S48_UNSPECIFIC;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_accept(s48_value sockfd_tagged, s48_value family)
{
  int  sockfd = s48_extract_fixnum (sockfd_tagged);
  switch(s48_extract_fixnum (family))
    {
    case AF_UNIX: 
      {
	struct sockaddr_un name;
	socklen_t namelen=sizeof(name);
	int newsockfd=accept(sockfd,(struct sockaddr *)&name,&namelen);

	if (newsockfd < 0)
	  {
	    if ((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN))
	      s48_raise_os_error_2(errno, sockfd_tagged, family);
	    if (! s48_add_pending_fd(sockfd, 1))/* 1 for is_input */
	      s48_raise_out_of_memory_error();
	    return S48_FALSE;
	  }
	fcntl(newsockfd, F_SETFL, O_NONBLOCK);
	return(s48_cons (s48_enter_fixnum (newsockfd),
			 s48_enter_string (name.sun_path)));
	break;
      }
    case AF_INET: 
      {
	struct sockaddr_in name;
	socklen_t namelen=sizeof(name);
	int newsockfd;
	s48_value result, sock_addr;
	S48_DECLARE_GC_PROTECT(2);
	newsockfd = accept (sockfd,
			    (struct sockaddr *)&name,&namelen);
	
	if (newsockfd < 0)
	  {
	    if ((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN))
	      s48_raise_os_error_2(errno, sockfd_tagged, family);
	    if (! s48_add_pending_fd(sockfd, 1))/* 1 for is_input */
	      s48_raise_out_of_memory_error();
	    return S48_FALSE;
	  }
	fcntl(newsockfd, F_SETFL, O_NONBLOCK);
	S48_GC_PROTECT_2 (result, sock_addr);
	sock_addr = s48_enter_unsigned_integer (ntohl(name.sin_addr.s_addr));
	
	result = s48_cons (sock_addr, s48_enter_fixnum (ntohs(name.sin_port)));

	result = s48_cons (s48_enter_fixnum (newsockfd),
			   result);
	S48_GC_UNPROTECT();
	return result;
	break;
      }
    default:
      s48_raise_argument_type_error (family);	    /* error unknown address family */
    }
}

s48_value make_addr (unsigned long net_s_addr, int net_s_port)
{
  s48_value result, sock_addr;
  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2 (result, sock_addr);
  sock_addr = s48_enter_unsigned_integer (ntohl (net_s_addr));
  
  result = s48_cons (sock_addr, s48_enter_fixnum (ntohs (net_s_port)));
  S48_GC_UNPROTECT();
  return result;
}
  
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_peer_name(s48_value sock, s48_value family)
{
  int sockfd = s48_extract_fixnum (sock);
  switch(s48_extract_fixnum (family))
    {
    case AF_INET: 
      {
	struct sockaddr_in name;
	socklen_t namelen=sizeof(name);
	int value=getpeername(sockfd,(struct sockaddr *)&name,&namelen);
	
	if (value < 0) s48_raise_os_error_2(errno, sock, family);
	
	return (make_addr (name.sin_addr.s_addr, name.sin_port));
	break;
      }
    default:
      s48_raise_argument_type_error (family);	/* error unknown address family */
    }
}
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scsh_shutdown (s48_value sch_sockfd, s48_value sch_how)
{
  int retval = shutdown (s48_extract_fixnum (sch_sockfd),
			 s48_extract_integer (sch_how));
  if (retval == -1)
    s48_raise_os_error_2 (errno, sch_sockfd, sch_how);
  return S48_UNSPECIFIC;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_socket_name(s48_value sock, s48_value family)
{
  int sockfd = s48_extract_fixnum (sock);
  switch(s48_extract_fixnum (family))
    {
    case AF_INET: 
      {
	struct sockaddr_in name;
	socklen_t namelen=sizeof(name);
	int value=getsockname(sockfd,(struct sockaddr *)&name,&namelen);
	
	if (value < 0) s48_raise_os_error_2(errno, sock, family);
	  
	return(make_addr (name.sin_addr.s_addr,
			  name.sin_port));
	break;
      }
    default:
      s48_raise_argument_type_error (family);	/* error unknown address family */
    }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_socket_pair(s48_value type)
{
  int sv[2];
  if( socketpair(PF_UNIX,s48_extract_integer (type),0,sv) ) 
    s48_raise_os_error_1(errno, type);
  
  return s48_cons (s48_enter_fixnum (sv[0]),
		   s48_cons (s48_enter_fixnum (sv[1]), S48_NULL));
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value
recv_substring(s48_value scm_sockfd, s48_value flags, s48_value buf, 
	       s48_value scm_start, s48_value scm_end)
{
  struct sockaddr_in name;
  socklen_t namelen=sizeof(name);
  int sockfd = s48_extract_fixnum (scm_sockfd);
  int start = s48_extract_fixnum (scm_start);
  int end = s48_extract_fixnum (scm_end);
  char* buf_part = s48_extract_string (buf) + start;
  
  int cc=recvfrom(sockfd,
		  buf_part, end-start, 
		  s48_extract_fixnum (flags),
		  (struct sockaddr *)&name, &namelen);
  
  if (cc >= 0)
    return (s48_cons (s48_enter_fixnum (cc), 
		      s48_cons (make_addr (name.sin_addr.s_addr,
					   name.sin_port),
				S48_NULL)));
	    
  
  if ((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN))
    s48_raise_os_error_5(errno, scm_sockfd, flags, buf, scm_start, scm_end);
  if (! s48_add_pending_fd(sockfd, 1))/* 1 for is_input */
    s48_raise_out_of_memory_error();
	return S48_FALSE;
}
      
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value send_substring(s48_value scm_sockfd,
			 s48_value scm_flags,
			 s48_value buf, 
			 s48_value scm_start, 
			 s48_value scm_end, 
			 s48_value scm_family, 
			 s48_value scheme_name)
{
  int n;
  int s = s48_extract_fixnum (scm_sockfd);
  int flags = s48_extract_fixnum (scm_flags);
  int start = s48_extract_fixnum (scm_start);
  int end = s48_extract_fixnum (scm_end);
  
  switch(s48_extract_fixnum (scm_family))
    {
    case 0:			/* only with connected sockets */
      {
        char* buf_part = s48_extract_string (buf) + start;
	n = send(s, buf_part, end-start, flags);
	break;
      }
    case AF_UNIX:
      {
	struct sockaddr_un name;
	int scheme_length=S48_STRING_LENGTH(scheme_name);
        char* buf_part = s48_extract_string (buf) + start;

	memset(&name, 0, sizeof(name));

	name.sun_family=AF_UNIX;	
	if (scheme_length>=(108-1)) /* save space for \0 */
	  return(-1);
	strncpy(name.sun_path,
		s48_extract_string(scheme_name),
		scheme_length);	/* copy to c string */
	name.sun_path[scheme_length]='\0'; /* add null */
	n = sendto(s, 
		   buf_part, end-start, 
		   flags,
		   (struct sockaddr *)&name, sizeof(name));
	break;
      }
    case AF_INET:
      {
	struct sockaddr_in name;
	u_long addr;
	u_short port;
        char* buf_part;
        S48_DECLARE_GC_PROTECT(2);

        S48_GC_PROTECT_2 (scheme_name,buf);
          
        addr = htonl (s48_extract_unsigned_integer (S48_CAR (scheme_name)));
        port = htons (s48_extract_fixnum (S48_CDR (scheme_name)));
        buf_part = s48_extract_string (buf) + start;

	memset(&name, 0, sizeof(name));

	name.sin_family=AF_INET;
	name.sin_addr.s_addr=addr;
	name.sin_port=port;

	n = sendto(s, 
		   buf_part, end-start, 
		   flags,
		   (struct sockaddr *)&name, sizeof(name));
      break;
      }
    default:
      s48_raise_argument_type_error (s48_extract_fixnum (scm_family));	
      /* error unknown address family */
    }
  S48_GC_UNPROTECT();
  if (n >= 0)
    return s48_enter_fixnum (n);

  if ((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN))
    s48_raise_os_error_7(errno,
			 scm_sockfd,
			 scm_flags,
			 buf, 
			 scm_start, 
			 scm_end, 
			 scm_family, 
			 scheme_name);
  
  if (! s48_add_pending_fd(s, 0))/* 0 for is_input */
    s48_raise_out_of_memory_error();
  
  return S48_FALSE;
}
  

      
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_getsockopt (s48_value s,
			     s48_value level,
			     s48_value optname)
{
  int optval;
  size_t optlen=sizeof(optval);
  
  if (getsockopt(s48_extract_fixnum (s),
		 s48_extract_fixnum (level),
		 s48_extract_fixnum (optname)
		 ,(char *)&optval,
		 &optlen) == -1)
    s48_raise_os_error_3 (errno, s, level, optname);
  return(s48_enter_fixnum (optval));

}

s48_value scheme_getsockopt_linger (s48_value s,
				    s48_value level,
				    s48_value optname)
{
  struct linger optval;
  size_t optlen=sizeof(optval);
  
  if (getsockopt(s48_extract_fixnum (s),
		 s48_extract_fixnum (level),
		 s48_extract_fixnum (optname),
		 (char *)&optval,
		 &optlen) == -1) {
    s48_raise_os_error_3 (errno, s, level, optname);
  }
  return s48_cons (s48_enter_fixnum (optval.l_onoff), 
		   s48_cons (s48_enter_fixnum (optval.l_linger), S48_NULL));
}

s48_value scheme_getsockopt_timeout (s48_value s,
				     s48_value level,
				     s48_value optname)
{
  struct timeval optval;
  size_t optlen=sizeof(optval);
  
  if (getsockopt(s48_extract_fixnum (s),
		 s48_extract_fixnum (level),
		 s48_extract_fixnum (optname),
		 (char *)&optval,
		 &optlen) == -1) {
    s48_raise_os_error_3 (errno, s, level, optname);
    }
  return(s48_cons (s48_enter_fixnum (optval.tv_sec),
		   s48_cons (s48_enter_fixnum (optval.tv_usec), S48_NULL)));
}
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
s48_value scheme_setsockopt (s48_value s,
			     s48_value level,
			     s48_value optname,
			     s48_value _optval)
{
  int optval = s48_extract_fixnum (_optval);
  if ((setsockopt(s48_extract_fixnum (s),
		  s48_extract_fixnum (level),
		  s48_extract_fixnum (optname),
		  (char *)&optval,sizeof(optval))) == -1)
    s48_raise_os_error_4 (errno, s, level, optname, _optval);
  return S48_UNSPECIFIC;
}

s48_value scheme_setsockopt_linger (s48_value s,
				    s48_value level,
				    s48_value optname,
				    s48_value onoff,
				    s48_value linger)
{
  struct linger optval;

  optval.l_onoff = s48_extract_fixnum (onoff);
  optval.l_linger = s48_extract_fixnum (linger);
  
  if ((setsockopt(s48_extract_fixnum (s),
		  s48_extract_fixnum (level),
		  s48_extract_fixnum (optname),
		  (char *)&optval,sizeof(optval))) == -1)
    s48_raise_os_error_5 (errno, s, level, optname, onoff, linger);
  return S48_UNSPECIFIC;
}

s48_value scheme_setsockopt_timeout (s48_value s,
				     s48_value level,
				     s48_value optname,
				     s48_value sec,
				     s48_value usec)
{
  struct timeval optval;
  optval.tv_sec = s48_extract_fixnum (sec);
  optval.tv_usec = s48_extract_fixnum (usec);
  
  if ((setsockopt(s48_extract_fixnum (s),
		  s48_extract_fixnum (level),
		  s48_extract_fixnum (optname),
		  (char *)&optval,sizeof(optval))) == -1)
    s48_raise_os_error_5 (errno, s, level, optname, sec, usec);
  return S48_UNSPECIFIC;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Routines for looking up hosts                                           */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

static s48_value host_info_type_binding = S48_FALSE;

s48_value host_ent2host_info (struct hostent * host)
{
  s48_value host_info = S48_FALSE;
  s48_value list = S48_FALSE;
  long * ptr;
  int i;
  S48_DECLARE_GC_PROTECT (2);
  
  if(host==NULL)
    {  
      return(s48_enter_fixnum (h_errno));
    }
  
  if (host_info_type_binding == S48_FALSE)
    {
      S48_GC_PROTECT_GLOBAL(host_info_type_binding); 
      host_info_type_binding = s48_get_imported_binding ("host-info-type");
    }

  S48_GC_PROTECT_2 (host_info, list);

  host_info = s48_make_record (host_info_type_binding);  
  S48_RECORD_SET (host_info, 0, s48_enter_string (host->h_name)); 
  
  ptr = (long *)host->h_aliases;
  list = S48_NULL;
  i = 0;
  while (*ptr) 
    { 
      list = s48_cons (s48_enter_string (host->h_aliases[i]), list);
      ptr++; 
      i ++;
    }

  S48_RECORD_SET (host_info, 1, list);  

  ptr = (long *)host->h_addr_list;
  list = S48_NULL;
  i = 0;
  while (*ptr)
    {
      list = 
	s48_cons (s48_enter_unsigned_integer (ntohl ((unsigned long) *(long *)(host->h_addr_list[i]))), 
		  list);
      ptr++;
      i++;
    }
    
  S48_RECORD_SET (host_info, 2, list);

  S48_GC_UNPROTECT ();
  return host_info;
}

s48_value scheme_host_address2host_info(s48_value addr_port)
{
  struct in_addr name;
  struct hostent *host;

  u_long  addr = htonl(s48_extract_unsigned_integer (S48_CAR (addr_port)));
  name.s_addr = addr;

  host=gethostbyaddr((char *)&name,sizeof(name),AF_INET);
  return (host_ent2host_info (host));
}


s48_value scheme_host_name2host_info(s48_value s_name)
{
  struct in_addr name;
  struct hostent *host;
  char * scheme_name = s48_extract_string (s_name);
  
  if ((name.s_addr=inet_addr(scheme_name)) != -1)
    host=gethostbyaddr((char *)&name,sizeof(name),AF_INET);
  else
    host=gethostbyname(scheme_name);
  
  return (host_ent2host_info (host));
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Routines for looking up networks                                        */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
static s48_value network_info_type_binding = S48_FALSE;

s48_value netent2net_info(struct netent *net)
{
    s48_value network_info = S48_FALSE;
    s48_value list = S48_FALSE;
    long * ptr;
    int i;
    S48_DECLARE_GC_PROTECT (2);
    
    if (net==NULL) return S48_FALSE;
    
    if (network_info_type_binding == S48_FALSE)
	{
	    S48_GC_PROTECT_GLOBAL(network_info_type_binding); 
	    network_info_type_binding = 
		s48_get_imported_binding ("network-info-type");
	}
    S48_GC_PROTECT_2 (network_info, list);
    
    network_info = s48_make_record (network_info_type_binding);  
    S48_RECORD_SET (network_info, 0, s48_enter_string (net->n_name)); 
    
    ptr = (long *)net->n_aliases;
    list = S48_NULL;
    i = 0;
    while (*ptr) 
	{ 
	    list = s48_cons (s48_enter_string (net->n_aliases[i]), list);
	    ptr++; 
	    i++;
	}
    
    S48_RECORD_SET (network_info, 1, list);  
    S48_RECORD_SET (network_info, 2, s48_enter_unsigned_integer (net->n_net)); 

    S48_GC_UNPROTECT ();

    return network_info;    
}

s48_value scheme_net_address2net_info(s48_value net_addr)
{
  struct netent *net;
  /* expects host byte order */
  net=getnetbyaddr(s48_extract_unsigned_integer (net_addr),AF_INET); 
  
  return netent2net_info (net);
}

s48_value scheme_net_name2net_info(s48_value scheme_name)
{
  struct netent *net = getnetbyname(s48_extract_string (scheme_name));
  return netent2net_info (net);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Routines for looking up services                                        */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/


static s48_value service_info_type_binding = S48_FALSE;

s48_value servent2service_info(struct servent *serv)
{
    s48_value service_info = S48_FALSE;
    s48_value list = S48_FALSE;
    long * ptr;
    int i;
    S48_DECLARE_GC_PROTECT (2);
    
    if (serv==NULL) return S48_FALSE;
    
    if (service_info_type_binding == S48_FALSE)
	{
	    S48_GC_PROTECT_GLOBAL(service_info_type_binding); 
	    service_info_type_binding = 
		s48_get_imported_binding ("service-info-type");
	}
    S48_GC_PROTECT_2 (service_info, list);
    
    service_info = s48_make_record (service_info_type_binding);  

    S48_RECORD_SET (service_info, 0, s48_enter_string (serv->s_name)); 
    
    ptr = (long *)serv->s_aliases;
    list = S48_NULL;
    i = 0;
    while (*ptr) 
	{ 
	    list = s48_cons (s48_enter_string (serv->s_aliases[i]), list);
	    ptr++; 
	    i++;
	}
    
    S48_RECORD_SET (service_info, 1, list);  
    S48_RECORD_SET (service_info, 2, s48_enter_fixnum (ntohs (serv->s_port)));
    S48_RECORD_SET (service_info, 3, s48_enter_string (serv->s_proto));

    S48_GC_UNPROTECT ();

    return service_info;    
}

s48_value scheme_serv_port2serv_info(s48_value in_port,
				     s48_value in_proto)
{
  struct servent *serv;
  char * proto;
  if (in_proto == S48_FALSE) proto = NULL;
  else proto = s48_extract_string (in_proto);
  
  serv = getservbyport(ntohs(s48_extract_fixnum (in_port)),
		       proto);
  
  return servent2service_info (serv);
}

s48_value scheme_serv_name2serv_info(s48_value in_name,
				     s48_value in_proto)
{
  struct servent *serv;
  char * proto;
  
  if (in_proto == S48_FALSE) proto = NULL;
  else proto = s48_extract_string (in_proto);
  
  serv = getservbyname(s48_extract_string (in_name),
		       proto);
  
  return servent2service_info (serv);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Routines for looking up protocols                                       */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

static s48_value protocol_info_type_binding = S48_FALSE;

s48_value protoent2protocol_info(struct protoent *proto)
{
    s48_value protocol_info = S48_FALSE;
    s48_value list = S48_FALSE;
    long * ptr;
    int i;
    S48_DECLARE_GC_PROTECT (2);
    
    if (proto==NULL) return S48_FALSE;
    
    if (protocol_info_type_binding == S48_FALSE)
	{
	    S48_GC_PROTECT_GLOBAL(protocol_info_type_binding); 
	    protocol_info_type_binding = 
		s48_get_imported_binding ("protocol-info-type");
	}
    S48_GC_PROTECT_2 (protocol_info, list);
    
    protocol_info = s48_make_record (protocol_info_type_binding);  

    S48_RECORD_SET (protocol_info, 0, s48_enter_string (proto->p_name)); 
    
    ptr = (long *)proto->p_aliases;
    list = S48_NULL;
    i = 0;
    while (*ptr) 
	{ 
	    list = s48_cons (s48_enter_string (proto->p_aliases[i]), list);
	    ptr++; 
	    i++;
	}
    
    S48_RECORD_SET (protocol_info, 1, list);  
    S48_RECORD_SET (protocol_info, 2, s48_enter_fixnum (proto->p_proto));

    S48_GC_UNPROTECT ();

    return protocol_info;    
}

s48_value scheme_proto_num2proto_info(s48_value in_proto)
{
  struct protoent *proto;

  proto=getprotobynumber(s48_extract_fixnum (in_proto));
  
  return protoent2protocol_info (proto);
}

s48_value scheme_proto_name2proto_info(s48_value in_name)
{
  struct protoent *proto=getprotobyname(s48_extract_string (in_name));

  return  protoent2protocol_info (proto);
}

void s48_init_network(void)
{
    S48_EXPORT_FUNCTION(scsh_socket);
    S48_EXPORT_FUNCTION(scheme_bind);
    S48_EXPORT_FUNCTION(scheme_connect);
    S48_EXPORT_FUNCTION(scsh_listen);
    S48_EXPORT_FUNCTION(scheme_accept);
    S48_EXPORT_FUNCTION(scheme_peer_name);
    S48_EXPORT_FUNCTION(scheme_socket_name);
    S48_EXPORT_FUNCTION(scsh_shutdown);
    S48_EXPORT_FUNCTION(scheme_socket_pair);
    S48_EXPORT_FUNCTION(recv_substring);
    S48_EXPORT_FUNCTION(send_substring);
    S48_EXPORT_FUNCTION(scheme_getsockopt);
    S48_EXPORT_FUNCTION(scheme_getsockopt_linger);
    S48_EXPORT_FUNCTION(scheme_getsockopt_timeout);
    S48_EXPORT_FUNCTION(scheme_setsockopt);
    S48_EXPORT_FUNCTION(scheme_setsockopt_linger);
    S48_EXPORT_FUNCTION(scheme_setsockopt_timeout);
    S48_EXPORT_FUNCTION(scheme_host_address2host_info);
    S48_EXPORT_FUNCTION(scheme_host_name2host_info);
    S48_EXPORT_FUNCTION(scheme_net_address2net_info);
    S48_EXPORT_FUNCTION(scheme_net_name2net_info);
    S48_EXPORT_FUNCTION(scheme_serv_port2serv_info);
    S48_EXPORT_FUNCTION(scheme_serv_name2serv_info);
    S48_EXPORT_FUNCTION(scheme_proto_num2proto_info);
    S48_EXPORT_FUNCTION(scheme_proto_name2proto_info);
}
