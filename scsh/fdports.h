/* Macros to access parts of Scheme ports from C. */
/* Copyright (c) 1995 by Olin Shivers. */

#define PortData_Fd(port_data)     (1+(s48_value*)StobData(port_data))
#define PortData_Closed(port_data) (2+(s48_value*)StobData(port_data))
#define PortData_Peek(port_data)   (3+(s48_value*)StobData(port_data))
#define PortData_Rev(port_data)    (4+(s48_value*)StobData(port_data))
#define PortData_OldRev(port_data) (5+(s48_value*)StobData(port_data))
/* #define PortData_Mode(port_data)   (6+(s48_value*)StobData(port_data)) */

#define Port_PortData(port) (1+(s48_value*)StobData(port))
#define PortFd(port)     (PortData_Fd(*Port_PortData(port)))
#define PortClosed(port) (PortData_Closed(*Port_PortData(port)))
#define PortRev(port)    (PortData_Rev(*Port_PortData(port)))
#define PortOldRev(port) (PortData_OldRev(*Port_PortData(port)))
/* #define PortMode(port)   (PortData_Mode(*Port_PortData(port))) */
