#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define streq(a,b) (strcmp((a),(b))==0)

static void usage(void) {
    fprintf(stderr, "Usage: %s [meta-arg] [vm-option+] [end-option scheme-args]\n"
            "meta-arg:    \\ <script file name>\n"
            "\n"
            "vm-option: -h <total heap size in words>\n"
            "           -s <stack size in words>\n"
            "           -o <object file name>\n"
            "\n"
            "end-option: -i <image file name>\n"
            "            -- (Terminates vm args.)\n"
            "            -a (Terminates vm args. Obsolete.)\n",
            "scshvm");}

static void bad_args(int reason) {
  /* fprintf(stderr, "reason : %s\n", reason); */
  usage();
  exit(1); 
}

char ** process_args(char **argv,
                     long *pheap_size,
                     long *pstack_size,
                     char **pobject_file,
                     char **pimage_name) {
    extern char **process_meta_arg(char **);

    /* Handle an initial \ <fname> meta-arg expansion. */
    while ( *argv && streq(*argv, "\\") ) {
        argv++;
        if( !*argv ) bad_args(0);	/* die */
        argv = process_meta_arg(argv);
        if( !argv ) {
            fprintf(stderr, "%s: \\ <fname> expansion failed.\n",
                    "scshvm");
            exit(1);}}

    for (; *argv; argv++)
         if (streq (argv[0],""))
           /* process_meta_arg inserts an empty argument for every space. 
              Skip it. */
           continue;
         else if( argv[0][0] != '-' )
           bad_args(1); /* die */
         else
           switch (argv[0][1]) {
            default:
                bad_args(2); /* die */
                break;

            case 'h': /* heapsize */
                argv++;
                if( !*argv ) bad_args(3); /* die */
                while (streq (argv[0],"")) argv++; /* Skip empty arguments */
                *pheap_size = atoi(*argv);
                if( *pheap_size <= 0 ) bad_args(4);
                break;

            case 's':
                argv++;
                if( !*argv ) bad_args(5); /* die */
                while (streq (argv[0],"")) argv++; /* Skip empty arguments */
                *pstack_size = atoi(*argv);
                if (*pstack_size <= 0) bad_args(6);
                break;

            case 'o': /* object file */
                argv++;
                if( !*argv ) bad_args(7); /* die */
                while (streq (argv[0],"")) argv++; /* Skip empty arguments */
                *pobject_file = *argv;
                break;

                /* These switches terminate arg scanning. */
            case 'i':
                argv++;
                if( !*argv ) bad_args(8); /* die */
                while (streq (argv[0],"")) argv++; /* Skip empty arguments */
                *pimage_name = *argv++;
                return argv;

            case '-':
            case 'a':
                argv++;
                return argv;}
    return argv;}
