{-
#
# digitgame
# Copyright 2005 Bart Massey <bart@cs.pdx.edu> 
# ALL RIGHTS RESERVED
# See the end of this file for licensing information
#
# Game with dice and digits (a wooden game owned
# by the Andreas Junghanns family)
#
-}

import Array
import List
import Ratio
import Char
import Maybe
import Data.HashTable

rules :: [ String ]
rules = ["Rules:",
	 "  A round is played solo, as follows",
         "    1. start with the sequence of digits 1..9",
         "    2. roll two 6-sided dice to obtain a sum 2..12",
         "    3. if there exists a set of digits in the sequence that add",
         "       up to that sum",
         "       3.1 delete some such set of digits",
         "       3.2 go to 2.",
         "    4. score is the remaining sequence, treated as a place value",
         "       number",
         "  Each player plays a round in turn.  If any player scores",
         "  0, they win immediately.  Otherwise, low scorer wins.",
         "",
         "This code gives equities for optimal solo play."
---	 "This code gives equities (in terms of fraction of loss=-1.0",
---	 ".. win=1.0) for optimal two-player play."
         ]

print_rules :: IO ()
print_rules = putStr $ foldl (++) "" $ map (++ "\n") rules

digits :: String
digits = "123456789"

rollsize :: Int
rollsize = 2 ^ length(digits);

rollnums :: [ Int ]
rollnums = [0..(rollsize - 1)]

rtable :: [ a ] -> Array Int a
rtable elems = array (0, rollsize - 1) (zip rollnums elems)

add_scores :: String -> String -> [ String ]
add_scores s "" = [ s ]
add_scores s t =
    (add_scores (s ++ [ head t ]) (tail t)) ++
    (add_scores s (tail t))

atoi :: String -> Integer
atoi "" = 0
atoi s = read s

itoa :: Integer -> String
itoa 0 = ""
itoa i = show i

all_scores :: Array Int String
all_scores =
    rtable $ map itoa $ reverse $ sort $ map atoi $ add_scores "" digits

dprob :: Array Int Rational
dprob = array (2, 12) $
              [(i,  ((toInteger(i) - 1) % 36)) | i <- [2.. 7]] ++
              [(i, ((13 - toInteger(i)) % 36)) | i <- [8..12]]

type Vtype = Rational

type Ptable = [ Vtype ]

score :: String -> Ptable
score state =
    map (score_prob (atoi state)) rollnums
    where
        score_prob v i = 
            let tv = atoi (all_scores ! i) in
                if v < tv
                then 1
                else if v == tv
                then 1%2
                else 0

--- Return a list of all possible die rolls that
--- achieve the given value in the given state.
rolls :: String -> Int -> [ String ]
rolls state rv =
    sortBy simpler (rolls' state [] rv)
    where
        rolls' _ ds 0 = [ reverse ds ]
        rolls' [] _ _ = []
        rolls' (s:ss) ds rv =
            let d = digitToInt s in
                if d > rv then [] else
                   rolls' ss (s:ds) (rv - d) ++
                   rolls' ss    ds       rv
        simpler a b =
            if (length a) < (length b) then LT
            else if (length a) > (length b) then GT
            else if a < b then LT
            else if a == b then EQ
            else GT
                
--- Wind up a list of ptables into a single ptable
--- by using the given combining operator entry-by-entry.
--- Give an error if there are no tables to combine, because
--- things may be different then.
ptable_combine :: (Vtype -> Vtype -> Vtype) -> [ Ptable ] -> Ptable
ptable_combine  _        [t] = t
ptable_combine op (t1:t2:ts) = zipWith op t1 $ ptable_combine op (t2:ts)

--- The value of a state at a given scoring threshold on a
--- given roll is the maximum value of its child states at
--- that threshold and roll, weighted appropriately.  For
--- rolls for which there is no child, the value of the
--- state at that roll is just its score.
value :: String -> Ptable
value state =
    let sc = (score state) in
        ptable_combine (+)
                       [ map (* (dprob ! rv))
                             (max_value sc (rolls state rv)) |
                         rv <- [2..12] ]
    where
       max_value sc rs = ptable_combine max $
                         sc : [ value (state \\ r) | r <- rs ]
            
{-


    &ptable value(string state) {
	if (!is_uninit(&tt[state]))
	    return &tt[state];
	printf("...valuing %s\n", state);
	&ptable s = &score(state);
	ptable v;
	if (exact)
	    v = (ptable){0, ...};
	else
	    v = (ptable){imprecise(0, 28), ...};
	for (int rv = 2; rv <= 12; rv++) {
	    &ptable try_roll(&ptable minv, int r, string s0, int j) {
		if (j >= length(s0))
		    return &minv;
		int d = s0[j] - '0';
		int r0 = r - d;
		if (r0 < 0)
		    return &minv;
                global string del(string state, int j) {
                    return
                        substr(state, 0, j) +
                        substr(state, j + 1, length(state) - j - 1);
                }
		string s1 = del(s0, j);
		void minimize(&ptable t)
		    /* For each threshold value, take
		       the *maximum* probability out
		       of t and minv */
		{
		    for (int i = 0; i < dim(t); i++)
			minv[i] = max(minv[i], t[i]);
		}
		if (r0 == 0) {
		    if (false)
			File::fprintf(stderr, ": %s %d = %s\n", state, rv, s1);
		    int d = dim(rt[state][rv]);
		    rt[state][rv][d] = s1;
		    minimize(&value(s1));
		} else {
		    minimize(&try_roll(&minv, r0, s1, j));
		}
		minimize(&try_roll(&minv, r, s0, j + 1));
		return &minv;
	    }
	    ptable s0 = s;
	    &ptable t = &try_roll(&s0, rv, state, 0);
	    for (int i = 0; i < dim(t); i++)
		v[i] += t[i] * dprob[rv];
	}
	tt[state] = v;
	return &v;
    }

    printf("initializing...\n");
    value(digits);
    printf("...done!\n");
    rtab = rt;
    return &tt;
}


string sminus(string s, string t) {
    int j = 0;
    string r = "";
    for (int i = 0; i < length(s); i++) {
	while(j < length(t) && s[i] > t[j])
	    j++;
	if (j < length(t) && s[i] == t[j])
	    continue;
	r += String::new(s[i]);
    }
    return r;
}

string get_input(string prompt) {
    while (true) {
	printf("%s ", prompt);
	File::flush(stdout);
	string s = chomp(gets());
	if (s == "q")
	    exit(0);
	if (s == "h" || s == "?") {
	    print_rules();
	    continue;
	}
	return s;
    }
}


&ptable[string] read_tt() {
    ptable[string] loadtt;
    twixt(file f = open("digitgame-table.txt", "r"); close(f)) {
	for (int i = 0; i < rollsize - 1; i++) {
	    string key = chomp(fgets(f));
	    ptable t = { [j] {
		assert(fscanf(f, "%g\n", &(vtype v)) == 1,
		       "bad ptable entry at %s/%d", key, j);
		return v;
	    }};
	    loadtt[key] = t;
	}
    }
    return &loadtt;
}

void write_tt(&ptable[string] tt) {
    twixt(file f = open("digitgame-table.txt", "w"); close(f)) {
	&string[*] keys = &hash_keys(tt);
	assert(dim(keys) == rollsize - 1, "strange ttable size %d", dim(keys));
	for (int i = 0; i < dim(keys); i++) {
	    fprintf(f, "%s\n", keys[i]);
	    &ptable t = &tt[keys[i]];
	    assert(dim(t) == rollsize, "strange table size");
	    for (int j = 0; j < dim(t); j++)
		fprintf(f, "  %v\n", t[j]);
	}
    }
}

void play(bool autoroll) {
    &ptable[string] tt = &read_tt();
    dev_srandom(64);
    string cur = digits;
    int threshold;
    do {
	string s = get_input("t>");
        threshold = atoi(s);
    } while(threshold <= 0 || threshold > rollsize - 1);
    printf("threshold = %s\n", all_scores[threshold]);
    while(true) {
	int rv;
	if (autoroll) {
	    rv = randint(6) + randint(6) + 2;
	} else {
	    do {
		string s = get_input("r>");
		rv = atoi(s);
	    } while(rv < 2 || rv > 12);
	}
	printf("%s ... %d\n", cur, rv);
	&string[*] rolls = &rtab[cur][rv];
	if (dim(rolls) == 0) {
	    int c = atoi(cur);
	    int t = atoi(all_scores[threshold]);
	    if (c < t)
		printf("You win!\n");
	    else if (t < c)
		printf("You lose.\n");
	    else
		printf("Tie game.\n");
	    printf("Thanks for playing!\n");
	    exit(0);
	}
	shuffle(&rolls);
	for (int i = 0; i < dim(rolls); i++)
	    printf("%c) %s -> %s\n",
		   i + 'a', sminus(cur, rolls[i]), rolls[i]);
	int c = 0;
	if (dim(rolls) > 1) {
	    while(true) {
		string s = get_input("c>");

		int parse_choice(string s) {
		    string t = "";
		    for (int i = 0; i < length(s); i++)
			if (s[i] != ' ')
			    t += String::new(s[i]);
		    if (length(t) == 1 && isalpha(t[0]))
			return tolower(t[0]) - 'a';
		    for (int i = 0; i < dim(rolls); i++)
			if (t == sminus(cur, rolls[i]))
			    return i;
		    return -1;
		}

		c = parse_choice(s);
		if (c != -1)
		    break;
		printf("%s?\n", s);
	    }
	} else {
	    printf("c> a\n");
	}
	string choice = rolls[c];
	Sort::qsort(&rolls,
		    bool func(string x, string y) {
	                return tt[x][threshold] < tt[y][threshold]; });
	for (int i = 0; i < dim(rolls); i++)
	    printf("%c%s -> %s = %.4f\n", choice==rolls[i] ? '*' : ' ',
		   sminus(cur, rolls[i]),
		   rolls[i],
		   tt[rolls[i]][threshold]);
	printf("\n");
	cur = choice;
	if (cur == "") {
	    printf("Perfect game!\n");
	    printf("thanks for playing!\n");
	    exit(0);
	}
    }
}

if (dim(argv) > 0) {
    argdesc argd = {
	args = {
	    { var = (arg_var.arg_flag) &(bool manual_roll = false),
	      name = "roll-manually",
	      abbr = 'r',
	      desc = "Player inputs each die roll."
	    },
	    { var = (arg_var.arg_flag) &(bool generate = false),
	      name = "generate",
	      abbr = 'g',
	      desc = "Generate needed tables."
	    },
	    { var = (arg_var.arg_flag) &(bool value_score = false),
	      name = "generate-score",
	      desc = "Tables give expected score, not win prob."
	    },
	    { var = (arg_var.arg_flag) &(bool value_imprecise = false),
	      name = "generate-imprecise",
	      desc = "Tables are from 28-bit floating point, not rationals."
	    },
	},
    };
    parseargs(&argd, &argv);
    if (generate) {
	if (manual_roll) {
	    usage(stderr);
	    exit(1);
	}
	&ptable[string] tt = &value_all(!value_score, !value_imprecise);
	write_tt(&tt);
	exit(0);
    }
    if (generate || value_score || value_imprecise) {
	usage(stderr);
	exit(1);
    }
    play(!manual_roll);
};

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated
# documentation files (the "Software"), to deal in the
# Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute,
# sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# 
# The above copyright notice and this permission notice shall
# be included in all copies or substantial portions of the
# Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
# 
# Except as contained in this notice, the names of the authors
# or their institutions shall not be used in advertising or
# otherwise to promote the sale, use or other dealings in this
# Software without prior written authorization from the
# authors.
-}
