IDEAL
MODEL small
STACK 100h
DATASEG

row_amount equ 7
column_amount equ 7
area equ row_amount*column_amount
zeroarray db 20 dup (0)
board_arr db area dup (0) ; 0=empty 10=bomb
justAnarray db 15 dup (14h)
topresentarray db area dup (0) ; 0=unseen 1=seen 2=flag 
how_many_bombs_input db 3,0,0,0,0
game_status db, 0
bombs_amount db 0
win_counter db 0
loss_counter db 0
action_chose db 0
arrow db '---> $'
expose_piece db 'expose piece$'
flag_piece db 'flag piece$'
choose_another db 'choose another piece$'
spaces db '     $'
yesorno db 0
yes db 'Yes$'
no db 'No$'
instructions_msg db 'fisrt you will choose a place to start and all the pieces around it will be       empty from bombs$'
how_many_bombs_msg db 'how many bombs do you want to place? (the units around it will be empty)', 10 , '$'
too_many_bombs_msg db 'you chose to place to many bombs', 10, 'try again', 10 , '$'
choose_action_msg db 'what do u want to do here?$'
wrong_input_msg db 'you have entered wrong imput please try again$'
already_seen_msg db 'this sqaure is laready seen you can not ineract with this piece, choose again$'
already_flagged_msg db 'you already flagged this piece, do you want to unflag it?$'
open_piece_msg db 'you can not choose an open piece, try again$'
explode_flagged_msg db 'this piece is flgged and you can not blow it up, try again$'
try_again_msg db 'try again$'
you_won_msg db 'you won the game, congrats!$'
you_lost_msg db 'you lost the game...$'
another_game_msg db 'do you want to play another game? (y for yes and n for no)$'
have_a_nice_day_msg db 'Have a nice day :)$'
win_counter_msg db 'you won: $'
loss_counter_msg db 'you lost: $'
con_msg db' games$'
lineFeed db 13, 10, '$'
space db ' $'
bomb_sign db 'B$'
empty_sign db 'E$'
flagSign db 'F$'

CODESEG
;this procedure allows the user to choose using the keyboard a position on the board
;input:topresentarray and board array
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
	
	push si
	push [bp+6] ;board arr
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
	inc dl ;go right one column
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
;input: board array, topresentarray and the chosen piece_changed
;output: changes (or not) the chosen piece's value
;purpose:to ineract with the chosen piece
proc int_with_piece
	push bp
	mov bp, sp
	push ax
	push dx
	push si
	push di
	mov [byte ptr action_chose], 0
	mov si, [bp+8] ;chosen piece
	mov di, [bp+6] ;topresentarray
	cmp [byte ptr si], 1 ;chekcs if the chosen piece is already seen
	je not_available
	mov dl, 1
	mov dh, column_amount+2
	mov ah, 2
	int 10h
	keyEnter equ 01Ch
	down equ 050h
	up equ 048h
presentAction:
	push di ;topresentarray
	push [bp+4] ;board arr
	call Present_Board
	mov dx, offset choose_action_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	cmp [byte ptr action_chose], 0
	jne maybeFlag
	mov dx, offset arrow
	mov ah, 9
	int 21h
	jmp exposeMarked
maybeFlag:
	mov dx, offset spaces
	mov ah, 9
	int 21h
exposeMarked:
	mov dx, offset expose_piece
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	cmp [byte ptr action_chose], 1
	jne maybeAnother
	mov dx, offset arrow
	mov ah, 9
	int 21h
	jmp flagMarked
maybeAnother:
	mov dx, offset spaces
	mov ah, 9
	int 21h
flagMarked:
	mov dx, offset flag_piece
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	cmp [byte ptr action_chose], 2
	jne not_another
	mov dx, offset arrow
	mov ah, 9
	int 21h
	jmp anotherMarked
not_another:
	mov dx, offset spaces
	mov ah, 9
	int 21h
anotherMarked:
	mov dx, offset choose_another
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
get_action:
	mov ah, 0
	int 16h
	cmp ah, up
	je actionUp
	cmp ah, down
	je actiondown
	cmp ah, keyEnter
	je enterPressed
	jmp get_action
actionUp:
	cmp [byte ptr action_chose], 0
	jne goup
	mov [byte ptr action_chose], 2
	jmp presentAction
goup:
	dec [byte ptr action_chose]
	jmp presentAction
actiondown:
	cmp [byte ptr action_chose], 2
	jne godown
	mov [byte ptr action_chose], 0
	jmp presentAction
godown:
	inc [byte ptr action_chose]
	jmp presentAction
enterPressed:
	mov al, [byte ptr action_chose]
	;jmp intercat1
	cmp al, 1
	je flag4
	cmp al, 0
	je explode2
	cmp al, 2
	je piece_changed
wrong_input2:
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset wrong_input_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	jmp get_action
flag4:
	cmp [byte ptr si], 2
	jne n12
presentYesOrNno:
	push di ;topresentarray
	push [bp+4] ;board arr
	call Present_Board
	mov dx, offset already_flagged_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
isitYes:
	cmp [byte ptr yesorno], 0
	jne notUp
	mov dx, offset arrow
	mov ah, 9
	int 21h
	jmp isItDown
notUp:
	mov dx, offset spaces
	mov ah, 9
	int 21h
isItDown:
	mov dx, offset Yes
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	cmp [byte ptr yesorno], 1
	jne notDown
	mov dx, offset arrow
	mov ah, 9
	int 21h
	jmp itwasyes
notDown:
	mov dx, offset spaces
	mov ah, 9
	int 21h
itwasyes:
	mov dx, offset no
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h

	mov ah, 0
	int 16h
	cmp ah, up
	je upItIs
	cmp ah, down
	je downItIs
	cmp ah, keyEnter
	je pressedEnter
	jmp presentYesOrNno

upItIs:
	cmp [byte ptr yesorno], 0
	jne justUp
	mov [byte ptr yesorno], 1
	jmp presentYesOrNno
justUp:
	dec [byte ptr yesorno]
	jmp presentYesOrNno
downItIs:
	cmp [byte ptr yesorno], 1
	jne justDown
	mov [byte ptr yesorno], 0
	jmp presentYesOrNno
justDown:
	inc [byte ptr yesorno]
	jmp presentYesOrNno
pressedEnter:
	mov al, [byte ptr yesorno]
	cmp al, 1
	je piece_changed
	cmp al, 0
	jne wrong_input2
	mov [byte ptr si], 0
	jmp piece_changed
n12:
	mov [byte ptr si], 2	
	jmp piece_changed
explode2:
	cmp [byte ptr si], 2
	je flag3
	;cmp [byte ptr si], 0
	;jne seen
	mov [byte ptr si], 1
	jmp piece_changed
flag3:
	mov dx, offset explode_flagged_msg
	mov ah, 9
	int 21h
	jmp piece_changed
not_available:
	mov dx, offset already_seen_msg
	mov ah, 9
	int 21h
piece_changed:
	pop di
	pop si
	pop dx
	pop ax
	pop bp
ret 6
endp int_with_piece


;input: board arr and topresentarray
;output: non (it shows the board)
;purpose: shows the unit's value if the unit is visible, if it's flagged a flag and if it's not visible a square
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
	mov di, [bp+4] ; board arr
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
;removed:
	;mov 
check:
	cmp [byte ptr si+bx], 1 ; 1 = seen
	jne flag
	;push si
	;mov si, [bp+8]
	;cmp [byte ptr si+bx], 1
	;je bombreveled
	;pop si

	mov dl, [byte ptr di+bx]
	cmp dl, 10 ; checks if ther piece is a bomb
	je b
	;cmp dl, 11
	;je removed
	add dl, '0' ;if it is seen and not a bomb or a flag it means it has a, this action turns it from ascii to the number itself 
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
;input:no input
;output: non
;purpose: to clear the screen

proc clear_board
	push ax
	mov ax, 3
	int 10h
	pop ax
	ret
endp clear_board
;input: none
;output: prints a square at cursor location
;purpose: to print a blank square
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
;input: a board to define
;output: sets random location for the bombs (the amount the player asks)
;purpose: to set location for bombs on the board
proc set_bombs
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	;[bp+6] a pressed location
	mov bx, [bp+4] ;start of board
	mov di, [bp+6]
	mov [byte ptr di], 2
	mov [byte ptr di+row_amount], 2
	mov [byte ptr di+row_amount+1], 2
	mov [byte ptr di+row_amount-1], 2
	mov [byte ptr di-row_amount], 2         ;if it gets a piece at the start it will make sure there are no bombs surrounding it (restored to zero later)
	mov [byte ptr di-row_amount-1], 2
	mov [byte ptr di-row_amount+1], 2
	mov [byte ptr di+1], 2
	mov [byte ptr di-1], 2
	mov dl, 1
	mov dh, column_amount+2
	mov ah, 2
	int 10h

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
	mov [byte ptr bx], 10
	pop cx
	loop place_bomb

	mov [byte ptr di], 0
	mov [byte ptr di+row_amount], 0
	mov [byte ptr di+row_amount+1], 0
	mov [byte ptr di+row_amount-1], 0
	mov [byte ptr di-row_amount], 0
	mov [byte ptr di-row_amount-1], 0
	mov [byte ptr di-row_amount+1], 0
	mov [byte ptr di+1], 0
	mov [byte ptr di-1], 0

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp set_bombs



;input:the offset of a location in the data seg, and the offset of the board arr
;output: non
;purpose: to count how many bombs are around each piece
proc count_around
 push bp
 mov bp, sp
 push ax
 push dx
 push di
 push si

 mov di, [bp+4];holds the checked location offset
 mov si, [bp+6];holds the board offset
 mov ax, di
 sub ax, si
 mov dl, ROW_AMOUNT
 div dl ;now, ah has x axis, al has y axis
 xor dl, dl ;dl will count the pieces around
 
checkTopRow: 
 cmp al, 0
 je checkMidRow
 cmp ah, 0
 je checkTopMid
 cmp [byte ptr di-ROW_AMOUNT-1], 10
 jne checkTopMid
 inc dl
checkTopMid:
 cmp [byte ptr di-ROW_AMOUNT], 10
 jne checkTopRight
 inc dl
checkTopRight:
 cmp ah, ROW_AMOUNT-1
 je checkMidRow
 cmp [byte ptr di-ROW_AMOUNT+1], 10
 jne checkMidRow
 inc dl
 
checkMidRow:
 cmp ah, 0
 je checkMidRight
 cmp [byte ptr di-1], 10
 jne checkMidRight
 inc dl
checkMidRight:
 cmp ah, ROW_AMOUNT-1
 je checkBotRow
 cmp [byte ptr di+1], 10
 jne checkBotRow
 inc dl
 
checkBotRow:
 cmp al, column_amount-1
 je placeNum
 cmp ah, 0
 je checkBotMid
 cmp [byte ptr di+ROW_AMOUNT-1], 10
 jne checkBotMid
 inc dl
checkBotMid:
 cmp [byte ptr di+ROW_AMOUNT], 10
 jne checkBotRight
 inc dl
checkBotRight:
 cmp ah, ROW_AMOUNT-1
 je placeNum
 cmp [byte ptr di+ROW_AMOUNT+1], 10
 jne placeNum
 inc dl
 
placeNum:
 mov [di], dl

 pop si
 pop di
 pop dx
 pop ax
 pop bp
 ret 4
endp count_around

;input:the board arr
;output: non (sets the board)
;purpose: to set the board (count how many bombs are around each piece)
proc set_present_board
 push bp
 mov bp, sp
 push ax
 push cx
 push dx
 push di
 
 mov di, [bp+4] ; board arr
 add di, AREA-1
count_around_Loop:
 cmp [byte ptr di], 10
 je bomb
 push [bp+4]
 push di
 call count_around
bomb:
 dec di
 cmp di, [bp+4]
 jae count_around_Loop


 pop di
 pop dx
 pop cx
 pop ax
 pop bp
 ret 2
endp set_present_board

;input: the array of the board_arr, and the topresent array
;output: non (changes a variable)
;purpose: to check if the ost the game
proc check_lose
	push bp
	mov bp, sp
	push si
	push di 
	push bx
	push dx
	mov si, [bp+6]
	mov di, [bp+4]
	;[bp+4] board_arr - di
	;[bp+6] topresentarray - si
	mov cx, area
check_loss:
	cmp [byte ptr di+bx], 10 ;checks if the checked location in the board arr is a bomb
	jne loopEn ;if not continue
	cmp [byte ptr si+bx], 1 ; if the it was a bomb, this checks if the bomb was seen in the topresentarray (means the player lost the game)
	jne loopEn
	mov [byte ptr game_status], 1
	mov cx, 1
	jmp loopEn
loopEn:
	inc bx
	loop check_loss

	pop dx
	pop bx
	pop di
	pop si
	pop bp 
	ret 4 
endp check_lose
;input: topresentarray offset, bombs amount
;output: non (changes a variable)
;purpose: to check if the player won the game
proc check_victory
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	mov ax, [bp+6]
	mov si, [bp+4] ;topresentarray
	
	xor dx, dx
	xor bx, bx
	mov cx, area
didWin:
	cmp [byte ptr si+bx], 1
	jne next
	inc dx
next:
	inc bx
	loop didWin
	mov cx, area 
	;mov al, [byte ptr bombs_amount]
	sub cx, ax
	cmp dx, cx
	jne didNotWin
	mov [game_status], 2
didNotWin:
	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4
endp check_victory



start:
	mov ax, @data
	mov ds, ax
	xor bx, bx

	mov cx, area
resetboard:
	mov [byte ptr board_arr+bx], 0
	mov [byte ptr topresentarray+bx], 0
	mov [byte ptr game_status], 0
	inc bx
	loop resetboard
	xor bx, bx
	push 1
	push offset topresentarray
	push offset board_arr
	call Choose_Place
	push offset board_arr
	call set_bombs
	push offset board_arr
	call set_present_board 

	mov cx, 1
gamePlay:
	
	

	push offset board_arr
	push offset topresentarray
	call Choose_Place
	push offset topresentarray
	push offset board_arr
	call int_with_piece
	
	push offset topresentarray
	push offset board_arr
	call check_lose
	xor ax, ax
	mov al, [byte ptr bombs_amount]
	push ax
	push offset topresentarray
	call check_victory
	cmp [byte ptr game_status], 1
	je gameOver
	cmp [byte ptr game_status], 2
	je gameOver
	cmp [byte ptr game_status], 0
	je gamePlay
gameOver:

	cmp [byte ptr game_status], 1
	je youLost
	push offset topresentarray
	push offset board_arr
	call present_board
	mov dl, 1
	mov dh, column_amount+2
	mov ah, 2
	int 10h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset you_won_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	inc [byte ptr win_counter]
	jmp toExit
youLost:
	push offset topresentarray
	push offset board_arr
	call present_board
	mov dl, 1
	mov dh, column_amount+2
	mov ah, 2
	int 10h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset you_lost_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	inc [byte ptr loss_counter]
toExit:
	mov dx, offset win_counter_msg
	mov ah, 9
	int 21h
	mov dl, [byte ptr win_counter]
	add dl, '0'
	mov ah, 2
	int 21h
	mov dx, offset con_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset loss_counter_msg
	int 21h
	mov dl, [byte ptr loss_counter]
	add dl, '0'
	mov ah, 2
	int 21h
	mov dx, offset con_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset another_game_msg
	mov ah, 9
	int 21h
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov ah, 7
	int 21h
	cmp al, 'y'
	je start
	cmp al, 'n'
	je exiting
	mov dx, offset wrong_input_msg
	mov ah, 9
	int 21h
	jmp toExit



exiting:
	mov dx, offset lineFeed
	mov ah, 9
	int 21h
	mov dx, offset have_a_nice_day_msg
	int 21h

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