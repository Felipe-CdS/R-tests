def checkNeighboursJackpot(board, xCoord, yCoord, xBoardDimensions, yBoardDimensions):
    
    neighbours = [board[yCoord-1][xCoord-1], board[yCoord-1][xCoord], board[yCoord-1][xCoord+1],
                  board[yCoord  ][xCoord-1],                          board[yCoord  ][xCoord+1],
                  board[yCoord+1][xCoord-1], board[yCoord+1][xCoord], board[yCoord+1][xCoord+1]];
            
    for i in range(0, 8):
        if(neighbours[i] != 7):
            return ([0,0]);
    return ([yCoord+1, xCoord+1]);


## Finds all the answer to the Ultimate Question of Life, the Universe, and Everything.
## The number 42
def lightSaberFinder(board, width, height):
    
    lightSaberCoords = [0, 0];
    
    ## we need that all the 42 surrounds are 7, so 42 on the board border can't be used.
    for j in range(1, height-1):
        for i in range(1, width-1, 2):
            if(board[j][i] == 42):
                holder = checkNeighboursJackpot(board, i, j, width, height);
                if(holder != [0,0]):
                    lightSaberCoords = holder;
                    return(lightSaberCoords);
            elif(board[j][i] == 7):
                if((board[j][(i-1)] == 42) and ((i-1) != 0)):
                    holder = checkNeighboursJackpot(board, i, j, width, height);
                    if(holder != [0,0]):
                        lightSaberCoords = holder;
                        return(lightSaberCoords);
                elif((board[j][(i+1)] == 42) and ((i+1) != (width-1))):
                    holder = checkNeighboursJackpot(board, i, j, width, height);
                    if(holder != [0,0]):
                        lightSaberCoords = holder;
                        return(lightSaberCoords);
                
    return(lightSaberCoords);

def getLineEntries(vector, width):
    result = [None] * width;
    
    lineEntry = input().split(" ");
    
    for i in range(0, width):
        result[i] = int(lineEntry[i]);
        
    return result;

## Main Board
dimensionsEntry = input().split(" ");
boardHeight = int(dimensionsEntry[0]);
boardWidth = int(dimensionsEntry[1]);

board = [[None] * boardWidth]* boardHeight;

for i in range(0, boardHeight):
    board[i] = getLineEntries(board[i], boardWidth);

result = lightSaberFinder(board, boardWidth, boardHeight);
print(result[0], result[1]);
