def sqinrec(length, width):
    squares = []
    while width:
        square_size = min(length, width)
        squares.append(square_size)

        if length > width:
            length -= square_size
        else:
            width -= square_size
            
        length, width = max(length, width), min(length, width)

    return (squares)

#main code
print (sqinrec(5,3))