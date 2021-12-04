import math

# No
# Don't say anything
# I had more interesting things to do than parse this in haskell

def is_winning(board):
    size = int(math.sqrt(len(board)))
    for x in range(size):
        if sum(board[x*size:(x+1)*size]) == -size:
            return True
    for y in range(size):
        colsum = 0
        for x in range(size):
            colsum += board[x*size+y]
        if colsum == -size:
            return True
    return False

def score(board):
    return sum(filter(lambda x: x >=0, board))

def number(board, n):
    changed = False
    for i in range(len(board)):
        if board[i] == n:
            changed = True
            board[i] = -1
    return changed, board

def solution():
    with open("input.txt") as f:
        numbers = list(map(int, f.readline().split(",")))
        boards = list(filter(lambda x: x != [], f.read().split("\n\n")))
        for i in range(len(boards)):
            boards[i] = list(map(int, boards[i].split()))
    
    winners = set()
    for n in numbers:
        for i in range(len(boards)):
            # I don't even want to try to optimise that. Yuck
            if i not in winners:
                changed, nboard = number(boards[i], n)
                if changed:
                    boards[i] = nboard
                    if is_winning(nboard):
                        winners.add(i)
                        if len(winners) == len(boards):
                            return n * score(nboard)           

if __name__ == "__main__":
    print(solution())