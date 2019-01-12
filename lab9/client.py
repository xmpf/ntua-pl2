import requests
import parse

url = 'https://courses.softlab.ntua.gr/pl2/2018b/exercises/palseq.php'

def parse_data(req):
    length = parse.search('length {:d}', req.text)[0]
    string = parse.search('<code class="block" id="question">{}</code>', req.text)[0]
    return (length, string)

# Dynamic Programming => Longest Palindromic Subsequence
def lps(length, string):
    dp = [[0 for x in range(length)] for y in range(length)]
    # strings of length=1 are palindrome
    for i in range(length):
        dp[i][i] = 1
    # build dp table using the lower diagonal
    for cl in range (2, length + 1):
        for i in range (length - cl + 1):
            j = i + cl - 1
            if (string[i] == string[j] and cl == 2):
                dp[i][j] = 2
            elif (string[i] == string[j]):
                dp[i][j] = dp[i + 1][j - 1] + 2
            else:
                dp[i][j] = max( dp[i][j - 1], dp[i + 1][j] )
    # longest palindromic substring
    return dp[0][length - 1]

def solve_problem(length, string):
    longest = lps(length, string)
    return (length - longest)

if __name__ == '__main__':
    sess = requests.Session()    
    request = sess.post(url)
    t = parse_data(request)
    
    rounds = 0
    while rounds < 10:
        print ('Round {}, length: {}, {}'.format(rounds, t[0], t[1]))
        ans = solve_problem(t[0], t[1])
        request = sess.post(url, data={'submit': 'Submit!', 'answer': ans})

        if 'class="right"' in request.text:
            print ("Right! :)")
            request = sess.post(url, data={'again': 'Play again!'})
            rounds += 1
            t = parse_data(request)
        else:
            print ("Wrong! :(")
            request = sess.post(url, data={'again': 'Continue!'})
