echo off
rem    Batch File to Compile, Link, Bind and Run the SORTDEMO demonstration
rem    program. This batch file can be run from your DOS prompt.
rem
cls
echo .
echo     *------------------* SORTDEMO demonstration program *-----------------*
echo     *                                                                     *
echo     * Please ensure that you have followed the installation instructions  *
echo     * for COBOL, which are found in the Getting Started section of your   *
echo     * COBOL Operating Guide.  This means that you will have used SETUP    *
echo     * to load both the DOS compiler and the OS/2 compiler and the         *
echo     * Microsoft Utilities. You will also have selected                    *
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
echo .
echo     *------------------* SORTDEMO demonstration program *-----------------*
echo     *                                                                     *
echo     * SORTDEMO must be "bound" to run under DOS as it makes use of OS/2   *
echo     * API calls.                                                          *
echo     *                                                                     *
echo     * To bind the program, the following files must be copied into the    *
echo     * current directory. They will have been loaded by SETUP into the     *
echo     * directory you selected for the COBOL system, and the one you        *
echo     * selected for the Microsoft Utilities.                               *
echo     *                                                                     *
echo     *         API.LIB                                                     *
echo     *         CBLBIND.LIB                                                 *
echo     *         CBLBIND.NOT                                                 *
echo     *         DOSCALLS.LIB                                                *
echo     *                                                                     *
echo     * Also, BIND.EXE must be available in the current directory or in a   *
echo     * directory on the DOS PATH.                                          *
echo     *                                                                     *
echo     * Press Ctrl+C to exit if these files are not present.                *
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
COBOL SORTDEMO OPTSPEED NOTRICKLE ;
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
echo     *---------------------------------------------------------------------*
echo     *                   Binding the SORTDEMO program                      *
echo     *---------------------------------------------------------------------*
echo on
BIND SORTDEMO CBLBIND.LIB DOSCALLS.LIB -N @CBLBIND.NOT
pause
echo off
if errorlevel == 1 goto binderr
cls
echo     *---------------------------------------------------------------------*
echo     *                         Running SORTDEMO                            *
echo     *---------------------------------------------------------------------*
echo on
SORTDEMO
echo off
cls
goto endsort
:nocob
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured while running the Compiler. Please ensure that you *
echo     * have installed all the necessary files.                             *
echo     *                                                                     *
echo     ***********************************************************************
goto endsort
:linkerr
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured while linking. Please ensure that you have         *
echo     * installed all the necessary files.                                  *
echo     *                                                                     *
echo     ***********************************************************************
goto endsort
:binderr
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured during the Bind process. Please ensure that you    *
echo     * have installed all the necessary files and that these files are in  *
echo     * the current working directory or accessible via the DOS PATH.       *
echo     *                                                                     *
echo     ***********************************************************************
goto endsort
:doanim
echo     ***********************************************************************
echo     *                                                                     *
echo     * This program cannot be Animated on DOS.                             *
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
