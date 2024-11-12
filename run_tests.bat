echo OFF
del .\tests\results.txt
setlocal EnableDelayedExpansion
for %%x in (.\tests\*.c) do (
	echo compiling %%x
	cargo run %%x
)
for %%x in (.\tests\*.s) do (
	echo gccing %%x
	gcc -m64 -o .\tests\%%~nx %%x
)
for %%x in (.\tests\*.s) do (
	echo running %%x
	.\tests\%%~nx 
	echo test %%~nx: !errorlevel!>>.\tests\results.txt
)
del .\tests\*.s
del .\tests\*.exe
fc .\tests\results.txt .\tests\expected.txt