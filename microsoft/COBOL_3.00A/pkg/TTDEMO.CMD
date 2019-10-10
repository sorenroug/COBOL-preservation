echo off
rem    Batch File to Compile, Link and Run the TICTAC demonstration program.
rem    This batch file can be run from your DOS prompt. If you specify the
rem    "animate" parameter to the batch file then the TICTAC program will be
rem    compiled and then ANIMATED.
cls
echo .
echo     *------------------* TICTAC demonstration program *-------------------*
echo     *                                                                     *
echo     * Please ensure that you have followed the installation instructions  *
echo     * for COBOL, which are found in the Getting Started section of your   *
echo     * COBOL Operating Guide.  This means that you have included the       *
echo     * directory containing your COBOL Compiler on your OS/2 PATH and you  *
echo     * will have set up the COBDIR environment variable to also include    *
echo     * the COBOL Compiler directory.                                       *
echo     *                                                                     *
echo     * Press Ctrl+C to exit if you have NOT properly installed your        *
echo     * COBOL Compiler, or copied the required files.                       *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo .
pause
if not exist TICTAC.CBL goto errtic
if %COBDIR%. == . goto errcob
:cobret
if %1. == animate. goto doanim
if %1. == ANIMATE. goto doanim
cls
echo     *---------------------------------------------------------------------*
echo     *          Compiling the TICTAC demonstration program                 *
echo     *---------------------------------------------------------------------*
echo on
PCOBOL TICTAC;
echo off
if errorlevel 1 goto nocob
cls
echo     *---------------------------------------------------------------------*
echo     *                   Linking the TICTAC program                        *
echo     *                                                                     *
echo     * Notice inclusion of ADIS.                                           *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo on
LINK TICTAC+%COBDIR%\ADIS+%COBDIR%\ADISKEY+%COBDIR%\ADISINIT/NOP,,,PCOBOL+DOSCALLS;
echo off
cls
echo     *---------------------------------------------------------------------*
echo     *                         Running TICTAC                              *
echo     *                                                                     *
echo     *  Can you beat the Program?    It is possible !!                     *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo on
TICTAC
echo off
echo .
cls
echo     *---------------------------------------------------------------------*
echo     *                                                                     *
echo     * Have you tried the Micro Focus COBOL ANIMATOR ?                     *
echo     *                                                                     *
echo     * For an example of how to get going with the ANIMATOR, rerun this    *
echo     * batch file with the "animate" parameter. That is, type the          *
echo     * following:  "TTDEMO ANIMATE"                                        *
echo     *                                                                     *
echo     *---------------------------------------------------------------------*
echo .
goto endtic
:doanim
cls
echo     *---------------------------------------------------------------------*
echo     *     Compiling the TICTAC demonstration program for Animation        *
echo     *---------------------------------------------------------------------*
echo on
PCOBOL TICTAC,NUL ANIM;
echo off
if errorlevel 1 goto nocob
cls
echo     *---------------------------------------------------------------------*
echo     *                    Animating the TICTAC program                     *
echo     *---------------------------------------------------------------------*
echo on
PANIMATE TICTAC;
echo off
if errorlevel 1 goto nocob
goto endtic
:nocob
echo     ***********************************************************************
echo     *                                                                     *
echo     * An error occured while running the Compiler. Please ensure that you *
echo     * have installed all the necessary files.                             *
echo     *                                                                     *
echo     ***********************************************************************
goto endtic
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
echo     * The TICTAC program is not in the current directory. Either change   *
echo     * directory or copy TICTAC.CBL from your issue disks.                 *
echo     *                                                                     *
echo     ***********************************************************************
:endtic
echo     *---------------------------------------------------------------------*
echo     *                   End of TICTAC Demonstration                       *
echo     *---------------------------------------------------------------------*
