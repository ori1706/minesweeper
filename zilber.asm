IDEAL
MODEL small
STACK 100h
DATASEG

row_amount equ 7
column_amount equ 7
area equ row_amount*column_amount

board_arr db area dup (0) ; 0=empty 1=bomb
present_board_array db area dup (0) ;numbers show how many bombs around point 
topresentarray db area dup (0) ; 0=unseen 1=bomb 2=flag 3=seen
how_many_bombs_input db 3,0,0,0,0
bombs_amount db ?
how_many_bombs_msg db 'how many bombs do you want to place?', 10 , '$'
too_many_bombs_msg db 'you chose to place to many bombs', 10, 'try again', 10 , '$'
choose_action_msg db 'what do u want to do here? (f to flag or e to explode)$'
wrong_input_msg db 'you have entered wrong imput please try again$'
already_seen_msg db 'this sqaure is laready seen you can not ineract with this piece, choose again$'
already_flagged_msg db 'you already flagged this piece, do you want to unflag it?(y for yes and n for no)$'
open_piece_msg db 'you can not explode an open piece, try again$'
explode_flagged_msg db 'this piece is flgged and you can not blow it up, try again$'
bomb_sign db 'B$'
empty_sign db 'E$'

CODESEG
;this procedure allows the user to choose using the keyboard a position on the board
;input: offset of board
;output: offset of the chosen place
proc Choose_Place
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	mov si, [bp+4] ;the board offset
	mov di, [bp+4] ;the looked at position
present:
	;push si
	;call Present_Board
	
	mov ax, di
	sub ax, si
	mov bl, ROW_AMOUNT
	div bl ;ah holds the x axis (0-9), al holds the y axis (0-9)
	mov dx, ax
	
	push dx ;keep it aside
	
	xchg dh, dl ;now, dh stores the y axis (row), dl stores the x axis (column)
	add dh, 2 ;go down two rows
	add dl, dl ;we are using two digits for each place, so we have to the double the visual movement
	inc dl ;go rigth one column
	xor bx, bx
	mov ah, 2
	int 10h ;moves the cursor, dh y axis, dl x axis
	mov bx, 10
	mov cx, 1
	mov ah, 9
	int 10h ;changes the next two digits to light green
	mov ah, 2
	mov dl, 219
	int 21h
	
	
	pop bx ;bh hold x axis, bl y axis (as dx did before)
	
	
	up equ 048h
	left equ 04Bh
	down equ 050h
	right equ 04Dh
	keyEnter equ 01Ch
	
waitForData:
	mov ah, 0
	int 16h
	cmp ah, right
	je MoveRight
	cmp ah, left
	je MoveLeft
	cmp ah, up
	je MoveUp
	cmp ah, down
	je MoveDown
	cmp ah, keyEnter
	je chosen
	jmp waitForData
	
MoveRight:
	cmp bh, ROW_AMOUNT-1
	je waitForData
	inc di
	jmp present
MoveLeft:
	cmp bh, 0
	je waitForData
	dec di
	jmp present
MoveUp:
	cmp bl, 0
	je waitForData
	sub di, ROW_AMOUNT
	jmp present
MoveDown:
	cmp bl, ROW_AMOUNT-1
	je waitForData
	add di, ROW_AMOUNT
	jmp present
	
chosen:
	mov [bp+6], di
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
endp Choose_Place
;input: selected piece
proc int_with_piece
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov si, [bp+4] ; si holds the chosen piece
	cmp [si], 1
	je alreadySeenMsg
	mov dx, offset choose_action_msg
	mov ah, 9
	int 21h
	mov ah, 07h ;input is in al
	int 21h
	cmp al, 'f'
	je flag1
	cmp al, 'e'
	je explode
wrong_input:
	mov dx, offset wrong_input_msg
	mov ah, 9
	int 21h
	jmp exitProc
flag1:
	cmp [si], 2
	jne n1
	mov dx, offset already_flagged_msg
	mov ah, 9
	int 21h
	mov ah, 07h
	int 21h
	cmp al, 'n'
	je exitProc
	cmp al, 'y'
	jne wrong_input
	mov [si], 0
n1:
	cmp [si], 1
	je pieceSeen
	mov [si], 2	
	jmp exitProc
explode:
	cmp [si], 2
	je flag2
	cmp [si], 0
	jne seen
	mov [si], 1
	jmp exitProc
flag2:
	mov dx, offset explode_flagged_msg
	mov ah, 9
	int 21h
	jmp exitProc
seen:
	mov dx, offset open_piece_msg
	mov ah, 9
	int 21h
exitProc:
	
pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
ret 2
endp int_with_piece

proc present_board
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov di, [bp+4] ; present (what is there)
	mov si, [bp+6] ; topresent what will the array present (seen flag or unseen)
	xor ax, ax
	xor bx, bx
	mov cx, area
present:
	cmp [byte ptr si+bx], 0 ; 0=unseen
	jne check
	call print_square
check:
	cmp [byte ptr si+bx],1 ; 1 = seen
	jne flag
	mov dl, [bytr ptr di+bx]
	add dl, '0'
	mov ah, 2
	int 21h
	jmp loopEnd
flag:
	mov dl, 'F'
	mov ah,2 
	int 21h
loopEnd:
	inc bx
	loop present


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop adx
	pop bp

	ret 4
endp present_board
proc clear_board
	push ax
	mov ax, 3
	int 10h
	pop ax
	ret
endp clear_board
proc print_square
	push ax
	push dx
	push cx
	mov ah, 2
	mov dl, 219
	int 21h
	;int 21h
	pop cx
	pop dx
	pop ax
	ret
endp print_square
proc set_bombs
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

input_bombs:
	mov dx, offset how_many_bombs_msg
	mov ah, 9
	int 21h
	mov ah, 0Ah
	mov dx, offset how_many_bombs_input
	int 21h
	cmp [byte ptr how_many_bombs_input+1], 1 ;how many digits entered
	je one_digit
two_digit:
	mov al, [byte ptr how_many_bombs_input+2] ;powered by 10 digit
	sub al, '0'
	mov ah, 10
	mul ah
	add al, [byte ptr how_many_bombs_input+3] ;powered by 1 digit
	sub ax, '0'
	jmp gotIt ;we got the actual bomb amount in al
one_digit:
	mov al, [byte ptr how_many_bombs_input+2]
	sub al, '0'
	;we got the actual bomb amount in al
gotIt:
	cmp al, area/2
	jb continue
	mov dx, offset too_many_bombs_msg
	mov ah, 9
	int 21h
	jmp input_bombs
continue:
	mov [bombs_amount], al
	mov cl, al
	xor ah, ah
place_bomb:
	push cx
generate_random_place:
	mov ah, 2Ch
	int 21h
	mov al, dl ;miliseconds
	mov ah, area
	mul ah ;ax has 0-99*area
	mov bl, 100
	div bl ;al has random place
	xor ah, ah
	mov bx, [bp+4] ;start of board
	add bx, ax ;the random generated place
	cmp [byte ptr bx], 0
	jne generate_random_place
	mov [byte ptr bx], 1
	pop cx
	loop place_bomb

	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
endp set_bombs
proc set_present_board
push bp
mov bp, sp
push ax
push bx
push cx
push dx
push di
push si

	mov bx, [bp+4] ;topresentarray
	mov di, [bp+6] ;board_arr
	xor ax, ax
	xor si, si
	mov cx, area
countLoop:
	push bx
	mov bx, si
	cmp [byte ptr di+bx], 1
	je bomb
	cmp al, 7
	je rightedge
	cmp al, 0
	je leftEdge
	cmp [byte ptr di+bx+row_amount], 1
	jne a1
	inc dx
a1:						
	cmp [byte ptr di+bx+(row_amount+1)], 1
	jne a2
	inc dx
a2:
	cmp [byte ptr di+bx+(row_amount-1)], 1
	jne a3
	inc dx
a3:
	cmp [byte ptr di+bx-row_amount], 1
	jne a4
	inc dx
a4:
	cmp [byte ptr di+bx-(row_amount+1)], 1
	jne a5
	inc dx
a5:
	cmp [byte ptr di+bx-(row_amount-1)], 1
	jne a6
	inc dx
a6:
	cmp [byte ptr di+bx-1], 1
	jne a7
	inc dx
a7:
	cmp [byte ptr di+bx+1], 1
	jne setPiece
	inc dx
	jmp setPiece

rightEdge:
	;push bx
	;mov bx, si
	cmp si, row_amount-1
	je rightTopCorner
	cmp si, area
	je rightBotoomCorner
	cmp [byte ptr di+bx-row_amount], 1
	jne c1
	inc dx
c1:
	cmp [byte ptr di+bx-(row_amount+1)],1
	jne c2
	inc dx
c2:
	add di, si
	cmp [byte ptr di-1], 1
	jne c3
	inc dx
c3:
	add di, si
	cmp [byte ptr di+row_amount], 1
	jne c4
	inc dx
c4:
	cmp [byte ptr di+bx+(row_amount-1)], 1
	jne setPiece
	inc dx
	jmp setPiece

leftEdge:
	push bx
	mov bx, si
	cmp si, 0
	je leftTopEdge
	cmp si, area-row_amount
	je leftBottomCorner
	cmp [byte ptr di+bx-row_amount], 1
	jne b1
	inc dx
b1:
	cmp [byte ptr di+bx-(row_amount-1)],1
	jne b2
	inc dx
b2:
	cmp [byte ptr di+bx+1], 1
	jne b3
	inc dx
b3:
	cmp [byte ptr di+bx+row_amount], 1
	jne b4
	inc dx
b4:
	cmp [byte ptr di+bx+(row_amount+1)], 1
	jne setPiece
	inc dx
	jmp setPiece

rightTopCorner:
	;push bx
	;mov bx, si
	cmp [byte ptr di+bx-1], 1
	jne d3
	inc dx
d3:
	cmp [byte ptr di+bx+row_amount], 1
	jne d4
	inc dx
d4:
	cmp [byte ptr di+bx+(row_amount-1)], 1
	jne setPiece
	inc dx
	jmp setPiece
rightBotoomCorner:
	push bx
	mov bx, si
	cmp [byte ptr di+bx-row_amount], 1
	jne g1
	inc dx
g1:
	cmp [byte ptr di+bx-(row_amount+1)], 1
	jne g2
	inc dx
g2:
	cmp [byte ptr di+bx-1], 1
	jne setPiece
	inc dx
	jmp setPiece

leftTopEdge:
	push bx
	mov bx, si
	cmp [byte ptr di+bx+1], 1
	jne e3
	inc dx
e3:
	cmp [byte ptr di+bx+row_amount], 1
	jne e4
	inc dx
e4:
	cmp [byte ptr di+bx+(row_amount+1)], 1
	jne setPiece
	inc dx
	jmp setPiece
leftBottomCorner:
	push bx
	mov bx, si
	cmp [byte ptr di+bx-row_amount], 1
	jne f1
	inc dx
f1:
	cmp [byte ptr di+bx-(row_amount-1)],1
	jne f2
	inc dx
f2:
	cmp [byte ptr di+bx+1], 1
	jne setPiece
	inc dx
	jmp setPiece
bomb:
	pop bx
	mov [byte ptr bx+si], 10
	jmp endLoop2
setPiece:
	pop bx
	mov [byte ptr bx+si], dl ; dl contains how much bombs are around the piece
endLoop2:
	inc si
	xor dx, dx
	loop countLoop

	



pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
ret 4
endp set_present_board

start:
	mov ax, @data
	mov ds, ax
	;push offset justANum
	push offset board_arr
	;push offset random_locations
	call set_bombs
	push offset present_board_array
	push offset board_arr
	call set_present_board

exit:
mov ax, 4c00h
int 21h
END start

;int 21h ah = 2 prints the char in dl, i suggest ascii code 219 (a full digit) ascii code 10 = down a row
;int 21h ah = 9 prints the string dx is pointing at
;int 21 - little text and files
;int 16 more text
;int 10 graphics