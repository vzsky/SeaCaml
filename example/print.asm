.data
str:
  .ascii "Hello world!\n"
  len = . - str                

.text
.globl _main
_main:
    movl   $0x2000004, %eax
    movl   $1, %edi
    leaq   str(%rip), %rsi  
    movq   $len, %rdx          
    syscall                      

    movl   $0x2000001, %eax 
    movl   $0, %edi
    syscall                      
