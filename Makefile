NAME=vamp1
CPU=atmega328p

.PHONY:	all
all:	$(NAME).hex

.PHONY:	prog
prog:	$(NAME).hex
	avrdude -p $(CPU) -c arduino -P /dev/serial/by-id/usb-1a86_USB2.0-Serial-if00-port0 -U flash:w:$(NAME).hex

$(NAME).hex:	$(NAME).asm *.inc *.txt
	avra -fI -o $(NAME).hex -m $(NAME).map -l $(NAME).lst $(NAME).asm

.PHONY:	clean
clean:
	rm -f $(NAME).cof $(NAME).eep.hex $(NAME).hex $(NAME).lst $(NAME).map $(NAME).obj
