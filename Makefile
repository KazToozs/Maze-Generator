
SRC	=	manu.ml

OPT	=	ocamlopt

BC	=	ocamlc	

RM	=	rm -f

OBJ	=	*.cmo *.cmi *.cmx

FLAGS	=	-w Aelz -warn-error A

NAME	=	step1

NAMEC	=	step1

all:	$(NAME)

$(NAME):
	$(OPT) -o $(NAME) $(FLAGS) $(SRC)
	$(BC) -o $(NAME).byte $(FLAGS) $(SRC)

clean:	
	$(RM) $(OBJ)

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAMEC)

re: fclean all
