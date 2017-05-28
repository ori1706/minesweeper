IDEAL
MODEL small
STACK 100h
DATASEG

row_amount equ 7
column_amount equ 7
area equ row_amount*column_amount
zeroarray db 8 dup (0)
board_arr db area dup (0) ; 0=empty 1=bomb
justAnarray db 7 dup (14h)
present_board_array db area dup (0) ;numbers show how many bombs around point 
topresentarray db area dup (0) ; 0=unseen 1=seen 2=flag 
how_many_bombs_input db 3,0,0,0,0
lose db 0
win db 0
bombs_amount db 0
how_many_bombs_msg db 'how many bombs do you want to place?', 10 , '$'
too_many_bombs_msg db 'you chose to place to many bombs', 10, 'try again', 10 , '$'
choose_action_msg db 'what do u want to do here? (f to flag ,e to explode and a to choose another piece)$'
wrong_input_msg db 'you have entered wrong imput please try again$'
already_seen_msg db 'this sqaure is laready seen you can not ineract with this piece, choose again$'
already_flagged_msg db 'you already flagged this piece, do you want to unflag it?(y for yes and n for no)$'
open_piece_msg db 'you can not choose an open piece, try again$'
explode_flagged_msg db 'this piece is flgged and you can not blow it up, try again$'
try_again_msg db 'try again$'
you_won_msg db 'you won the game, congrats!$'
you_lost_msg db 'you lost the game...$'
another_game_msg db 'do you want to play another game?$'
have_a_nice_day_msg db 'Have a nice day :)$'
lineFeed db 13, 10, '$'
succecfulMoves db ,0
bombsFlagged db 0
space db ' $'
bomb_sign db 'B$'
empty_sign db 'E$'
flagSign db 'F$'

CODESEG
;this procedure allows the user to choose using the keyboard a position on the board
;input: offset of the array to present and the offset of the array that holds the amount of bombs near each piece
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
	
	mov si, [bp+4] ;the board to present offset
	mov di, [bp+4] ;the looked at position
present2:
	;push [bp+8]
	push si
	push [bp+6] ;amount of bombs near each piece
	call Present_Board
	
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
	mov cx, 2
	mov ah, 9
	int 10h ;changes the next two digits to light green
	mov ah, 2
	mov dl, 219
	int 21h
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
	jmp present2
MoveLeft:
	cmp bh, 0
	je waitForData
	dec di
	jmp present2
MoveUp:
	cmp bl, 0
	je waitForData
	sub di, ROW_AMOUNT
	jmp present2
MoveDown:
	cmp bl, ROW_AMOUNT-1
	je waitForData
	add di, ROW_AMOUNT
	jmp present2
	
chosen:
	mov [bp+8], di
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
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
	mov dl, 1
	mov dh, column_amount+2
	mov ah, 2
	int 10h
	mov si, [bp+4] ; si holds the chosen piece
	cmp [byte ptr si], 1 ;chekcs if the chosen piece is already seen
	je seen
choose_action_input:
	mov dx, offset choose_action_msg
	mov ah, 9
	int 21h
	mov ah, 07h ;input is in al
	int 21h
	cmp al, 'f'
	je flag1
	cmp al, 'e'
	je explode
	cmp al, 'a'
	je exitProc
wrong_input:
	mov dx, offset wrong_input_msg
	mov ah, 9
	int 21h
	mov dx, offset try_again_msg
	mov ah, 9
	int 21h
	jmp choose_action_input
flag1:
	cmp [byte ptr si], 2
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
	mov [byte ptr si], 0
	jmp exitProc
n1:
	mov [byte ptr si], 2	
	jmp exitProc
explode:
	cmp [byte ptr si], 2
	je flag2
	cmp [byte ptr si], 0
	jne seen
	mov [byte ptr si], 1
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
;input: the arrau that holds how many bombs surrounf each unit and the array to present_board and the board
;output: non
proc present_board
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	call clear_board
	mov di, [bp+4] ; present (what is there, the maount of bobms...)
	mov si, [bp+6] ; topresent what will the array present (seen flag or unseen)
	;[bp+8] is the board
	xor ax, ax
	xor bx, bx
	mov dl, 1
	mov dh, 2
	mov ah, 2
	int 10h
	mov cx, column_amount
present1:
	push cx
	mov cx, row_amount
present3:

	cmp [byte ptr si+bx], 0 ; 0=unseen
	jne check
	call print_square
	jmp loopEnd
;bombreveled:
	;mov [byte ptr lose], 1
	;pop si
	;jmp loopEnd
check:
	cmp [byte ptr si+bx], 1 ; 1 = seen
	jne flag
	;push si
	;mov si, [bp+8]
	;cmp [byte ptr si+bx], 1
	;je bombreveled
	;pop si
	mov dl, [byte ptr di+bx]
	cmp dl, 10
	je b
	add dl, '0'
	mov ah, 2
	int 21h
	mov dx, offset space
	mov ah, 9
	int 21h
	jmp loopEnd
b:
	mov dl, 'B'
	mov ah, 2
	int 21h
	mov dx, offset space
	mov ah, 9
	int 21h
	jmp loopEnd
flag:
	mov dx, offset flagSign
	mov ah, 9
	int 21h
	mov dx, offset space
	mov ah, 9
	int 21h
loopEnd:
	inc bx
	loop present3

	mov dx, offset lineFeed
	mov ah, 9
	int 21h 
	mov dx, offset space
	mov ah, 9
	int 21h
	pop cx
	loop present1

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
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
	mov cx, 7
	mov ah, 2
	mov dl, 219
	int 21h
	int 21h
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

	mov di, [bp+4] ;board_arr
	mov bx, [bp+6] ;present array
	xor ax, ax
	xor si, si
	mov cx, area
countLoop:
	push bx
	mov bx, si
	cmp [byte ptr di+bx], 1
	je bomb
	cmp al, row_amount-1 ; cheks if right corner
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
	cmp [byte ptr di+bx-1], 1
	jne c3
	inc dx
c3:
	add di, si
	cmp [byte ptr di+bx+row_amount], 1
	jne c4
	inc dx
c4:
xor ax, ax
	cmp [byte ptr di+bx+(row_amount-1)], 1
	jne setPiece
	inc dx
	jmp setPiece

leftEdge:
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
	inc al
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


	proc manage_game2
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov di, [bp+4]
	mov si, [bp+6]
	;[bp+4] - board array - di
	;[bp+6] topresentarray - si
	;[bp+8] present array
	push di
	call set_bombs
	push [bp+8]
	push di
	call set_present_board

	mov cl, 0FFh
gamePlay:
	
	push di
	push [bp+8]
	push si
	call Choose_Place
	call int_with_piece
	push si
	push di
	call check_game
	pop ax
	cmp ax, 1
	je loss
	cmp ax, 2
	je wonGame
	jmp gamePlay
	;xor bx, bx
	;mov cx, area
;check_loss:
	;cmp [byte ptr di+bx], 1
	;jne loopEn
	;cmp [byte ptr si+bx], 1
	;jne notSeen
	;mov [lose], 1
	;mov cx, 1
	;jmp loopEn
;notSeen:
	;cmp [byte ptr si+bx], 10
	;jne loopEn
	;inc dx
;Check_if_won:
	;cmp dl, [ byte ptr bombs_amount]
	;jne loopEn
	;mov [win], 1
	;jmp loopEn
;loopEn:
	;inc bx
	;loop check_loss

	;cmp [lose], 1
	;jne maybeWon
	;jmp lost_or_won
loss:
	mov dx, offset you_lost_msg
	mov ah,9 
	int 21h
	jmp endProc
wonGame:
	mov dx, offset you_won_msg
	mov ah, 9
	int 21h
endProc:
	ret 8
	endp manage_game2

proc check_game
	push bp
	mov bp, sp
	mov si, [bp+6]
	mov di, [bp+4]
	;[bp+4] board_arr - di
	;[bp+6] topresentarray - si
check_loss:
	cmp [byte ptr di+bx], 1
	jne loopEn
	cmp [byte ptr si+bx], 1
	jne notSeen
	mov [lose], 1
	jmp loopEn
notSeen:
	cmp [byte ptr si+bx], 10
	jne loopEn
	inc dx
Check_if_won:
	cmp dl, [ byte ptr bombs_amount]
	jne loopEn
	mov [win], 1
loopEn:
	inc bx
	loop check_loss

	cmp [lose], 1
	je lostGame
	cmp [win], 1
	je gamewon
	jmp notWonOrLost
lostGame:
	mov [byte ptr bp+6], 1
gameWon:
	mov [ byte ptr bp+6], 2
notWonOrLost:
	mov [byte ptr bp+6], 0
	ret 4
endp check_game
start:
	mov ax, @data
	mov ds, ax
	jmp fckoff
	mov cx, 1
playing:
	push offset present_board_array
	push offset topresentarray
	push offset board_arr
	call manage_game2
askAnotherGame:
	mov dx, offset another_game_msg
	mov ah,9 
	int 21h
	mov ah, 07h ;input is in al
	int 21h
	cmp al, 'y'
	jne notYes
	jmp playing
notYes:
	cmp al, 'n'
	jne notNo
	jmp endGame
notNo:
	mov dx, offset wrong_input_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	int 21h
endGame:
	mov dx, offset have_a_nice_day_msg
	mov ah, 9
	int 21h


	;push offset justANum
	;push offset board_arr
	;push offset random_locations
	;call set_bombs
	;push offset present_board_array
	;push offset board_arr
	;call set_present_board
	;mov cx, 1000
fckoff:	
	push offset board_arr
	call set_bombs
	push offset present_board_array
	push offset board_arr
	call set_present_board
	mov cx, 1
fcking_play:

	
	;push offset present_board_array
	;push offset topresentarray
	;call manage_game
	;push [bp+4] ; offset of the board arr
	;push [bp+6] ; offset of what to present (topresentarray)
	;push [bp+8] ; offset of the array that holds the amount of bombs that surround a piece
	push offset board_arr
	push offset present_board_array
	push offset topresentarray
	
	call Choose_Place
	call int_with_piece
	inc cx
	loop fcking_play
	;push offset topresentarray
	;push offset present_board_array
	;call present_board



exit:
mov ax, 4c00h
int 21h
END start

;int 21h ah = 2 prints the char in dl, i suggest ascii code 219 (a full digit) ascii code 10 = down a row
;int 21h ah = 9 prints the string dx is pointing at
;int 21 - little text and files
;int 16 more text
;int 10 graphics
;note to self: left edge counting is wrong