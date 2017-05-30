@echo off
setlocal
set rebarscript=%~f0
call escript "%rebarscript:.cmd=%" %*
