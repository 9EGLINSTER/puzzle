section .data
	geglinster:     db '9EGLINSTER',10
	geglinsterLen:  equ $-geglinster

section .text
	global _start

_start:
	mov eax,4
	mov ebx,1
	mov ecx,geglinster
	mov edp,geglinsterLen
	                     
    int 80h
	mov eax,1
	mov ebx,0
	int 80h;