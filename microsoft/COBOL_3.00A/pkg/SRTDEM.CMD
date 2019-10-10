echo off
rem    Batch File to Compile, Link and Run the SORTDEMO demonstration program.
rem    This batch file can be run from your OS/2 prompt. If you specify the
rem    "animate" parameter to the batch file then the SORTDEMO program will be
rem    compiled, linked and then ANIMATED.
rem
cls
echo .
echo     *------------------* SORTDEMO demonstration program *-----------------*
echo     *                                                                     *
echo     * Please ensure that you have followed the installation instructions  *
echo     * for COBOL, which are found in the Getting Started section of your   *
echo     * COBOL Operating Guide.  This means that you will have used SETUP    *
echo     * to load the OS/2 compiler and the Microsoft Utilities and selected  *
echo     * the default configuration. SETUP will create the required system    *
echo     * configuration, which you must have activated by rebooting the       *
echo     * operating system.                                                   *
echo     *                                                                     *
echo     * Press Ctrl+C to exit if you have NOT properly installed your        *
echo     * COBOL Compiler, or copied the required files.                       *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo .
pause
cls
if not exist SORTDEMO.CBL goto errtic
if %COBDIR%. == . goto errcob
:cobret
if %1. == animate. goto doanim
if %1. == ANIMATE. goto doanim
cls
echo     *---------------------------------------------------------------------*
echo     *          Compiling the SORTDEMO demonstration program               *
echo     *---------------------------------------------------------------------*
echo on
PCOBOL SORTDEMO OPTSPEED NOTRICKLE ;
echo off
if errorlevel 1 goto nocob
cls
echo     *---------------------------------------------------------------------*
echo     *                   Linking the SORTDEMO program                      *
echo     *---------------------------------------------------------------------*
echo on
LINK SORTDEMO/NOP/NOD,,,PCOBOL+DOSCALLS ;
echo off
if errorlevel == 1 goto linkerr
if not exist SORTDEMO.EXE goto linkerr
cls
echo     *---------------------------------------------------------------------*
echo     *                         Running SORTDEMO                            *
echo     *---------------------------------------------------------------------*
echo on
SORTDEMO
echo off
cls
echo .
echo     *---------------------------------------------------------------------*
echo     *                                                                     *
echo     * For an example of how to Animate this program, and others using     *
echo     * calls to OS/2 API functions, rerun this demo, but with a parameter  *
echo     * of ANIMATE (i.e. enter SRTDEM ANIMATE)                              *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo .
goto endsort
:doanim
cls
echo     *---------------------------------------------------------------------*
echo     *     Compiling the SORTDEMO demonstration program for Animation      *
echo     *---------------------------------------------------------------------*
echo on
PCOBOL SORTDEMO,NUL ANIM;
echo off
if errorlevel 1 goto nocob
echo     *---------------------------------------------------------------------*
echo     *                     SORTDEMO compiled succesfully                   *
echo     *---------------------------------------------------------------------*
pause
cls
echo     *---------------------------------------------------------------------*
echo     *                    Animating the SORTDEMO program                   *
echo     *                                                                     *
echo     * Since the Animator does not know about OS/2 API function calls,     *
echo     * and SORTDEMO uses these calls, it is necessary to inform Animator   *
echo     * about the calls. Once the Animation has started, this can be done   *
echo     * anytime before an OS/2 API function call is encountered, or         *
echo     * immediatley after an attempt to make such a call has caused         *
echo     * Animator to return an error 173.                                    *
echo     *                                                                     *
echo     * To install the calls, use the Animator Do facility to call the      *
echo     * appropriate install-function sub-program. e.g. enter                *
echo     *     D(o)   call "cobolapi.dll"                                      *
echo     * where cobolapi.dll is assumed to be in the COBOL system directory,  *
echo     * which is registered on the OS/2 LIBPATH.                            *
echo     *                                                                     *
echo     * Once the calls are installed, you may animate the program normally. *
echo     * The user screen will be "flashed" whenever a call is made to a      *
echo     * function which may affect the screen contents.                      *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
pause
echo on
PANIMATE SORTDEMO
echo off
echo     *---------------------------------------------------------------------*
echo     * If you terminated the Animation without completing the program your *
echo     * screen may be in the wrong mode. You can restore the default mode   *
echo     * using the "MODE" command. For example, enter                        *
echo     *                  MODE CO80                                          *
echo     *---------------------------------------------------------------------*
goto endsort
:nocob
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured while running the Compiler. Please ensure that you *
echo     * have installed all the necessary files correctly.                   *
echo     *                                                                     *
echo     ***********************************************************************
goto endsort
:linkerr
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured while linking. Please ensure that you have         *
echo     * installed all the necessary files correctly.                        *
echo     *                                                                     *
echo     ***********************************************************************
goto endsort
:errcob
echo     ***********************************************************************
echo     *                                                                     *
echo     * WARNING!   COBDIR environment variable not set                      *
echo     *                                                                     *
echo     ***********************************************************************
goto cobret
:errtic
echo     ***********************************************************************
echo     *                                                                     *
echo     * The SORTDEMO program is not in the current directory. Either change *
echo     * directory or copy SORTDEMO.CBL from your issue disks.               *
echo     *                                                                     *
echo     ***********************************************************************
:endsort
echo     *---------------------------------------------------------------------*
echo     *                   End of SORTDEMO Demonstration                     *
echo     *---------------------------------------------------------------------*
