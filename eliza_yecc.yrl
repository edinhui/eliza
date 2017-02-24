Nonterminals
scripts script decom_pattern decom_patterns reasemb_patterns reasemb_pattern words priority name.


Terminals
word split initial final quit pre post synon key decomp reasmb number goto.

Rootsymbol scripts.
Endsymbol '$end'.


scripts -> script.
scripts -> script scripts.
script -> initial split word words.
script -> final split word words.
script -> quit split word.
script -> quit split quit.
script -> pre split word words.
script -> post split word words.
script -> synon split word words.
script -> key split name priority decom_patterns.
words -> word.
words -> word words.
priority -> '$empty'.
priority -> number.
decom_patterns -> decom_pattern.
decom_patterns -> decom_pattern decom_patterns.
decom_pattern -> decomp split word  reasemb_patterns.
decom_pattern -> decomp split word words  reasemb_patterns.
reasemb_patterns -> reasemb_pattern.
reasemb_patterns -> reasemb_pattern  reasemb_patterns.
reasemb_pattern -> reasmb split goto name.
reasemb_pattern -> reasmb split word words. 
name -> word.
