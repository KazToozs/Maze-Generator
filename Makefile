
SRC	=	manu.ml

OPT	=	ocamlopt

BC	=	ocamlc	

RM	=	rm -f

OBJ	=	*.cmo *.cmi *.cmx

NAME	=	step1

NAMEC	=	step1

all:	$(NAME)

$(NAME):
	$(OPT) -o $(NAME) $(SRC)
	$(BC) -o $(NAME).byte $(SRC)

clean:	
	$(RM) $(OBJ)

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAMEC)

re: fclean all
