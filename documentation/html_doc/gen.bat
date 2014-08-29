@echo off
del lib_dic /q /s
doxygen Doxyfile.doxy
start lib_doc\html\index.html