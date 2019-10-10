#include <dos.h>
#include <stdio.h>
#include <fcntl.h>
#include <ios1.h>

extern char _DOS,**_ARGV,**environ;
extern int _ARGC;

#ifndef TINY
extern int _stack,_fmode,_iomode;
extern struct UFB _ufbs[];
#endif

/**
*
* name         initio - open files (extracted from _main.c)
*
* synopsis     initio();
*
* description	This function performs the standard file pre-processing for
*		a Lattice C program.
*
**/
void LINITIO()
{
#ifndef TINY
int x,y;
#endif

#if MSDOS1
#ifndef TINY
FILE *fp0, *fp1, *fp2;
extern char _iname[],_oname[];
extern int _bufsiz;
char *getmem();
#endif
#endif

/*
*
* Open standard files
*
*/
#ifndef TINY
#if MSDOS1
if(_DOS < 2)
	{
	fp0 = freopen(_iname,"r",stdin);
	if(_oname[0] != '>') fp1 = freopen(_oname,"w",stdout);
	else fp1 = freopen(&_oname[1],"a",stdout);
	fp2 = freopen("","a",stderr);
	if (fp2 == NULL) _exit(1);
	if (fp0 == NULL)
	   {
	   fputs("Can't open stdin file\n", fp2);
	   exit(1);
	   }
	setbuf(fp0, getmem(_bufsiz));	/* set stdin buffered */
	fp0->_flag &= ~_IOMYBUF;	/* allow rlsmem if later set unbuff'd */
	if (fp1 == NULL)
	   {
	   fputs("Can't open stdout file\n", fp2);
	   exit(1);
	   }
	}
else
#endif
	{
	x = (_fmode) ? 0 : _IOXLAT;
	stdin->_file = 0;
	stdin->_flag = _IOREAD | x;
	stdout->_file = 1;
	stdout->_flag = _IOWRT | x;
	if ((getfc(1,&y) == 0) && (y & 0x80)) stdout->_flag |= _IONBF;
	stderr->_file = 2;
	stderr->_flag = _IORW | _IONBF | x;
	stdaux->_file = 3;
	stdaux->_flag = _IORW | x;
	stdprt->_file = 4;
 	stdprt->_flag = _IOWRT | x;
	}
#endif

}
