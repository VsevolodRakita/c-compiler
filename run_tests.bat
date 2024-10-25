echo OFF
del .\tests\results.txt
setlocal EnableDelayedExpansion
for %%x in (.\tests\*.c) do (
	cargo run %%x
)
for %%x in (.\tests\*.s) do (
	gcc -m64 -o .\tests\%%~nx %%x
)
for %%x in (.\tests\*.s) do (
	.\tests\%%~nx 
	echo test %%~nx: !errorlevel!>>.\tests\results.txt
)
del .\tests\*.s
del .\tests\*.exe
fc .\tests\results.txt .\tests\expected.txt