Nonterminals
scripts script decom_pattern decom_patterns reasemb_patterns reasemb_pattern words priority name.


Terminals
word split initial final quit pre post synon key decomp reasemb number goto.

Rootsymbol scripts.

scripts -> script.
scripts -> script scripts.
script -> initial split words.
script -> final split words.
script -> quit split words.
script -> pre split word word.
script -> post split word word.
script -> synon split word words.
script -> key split name priority decom_patterns.
words -> word.
words -> word words.
priority -> '$empty'.
priority -> number.
decom_patterns -> decom_pattern decom_patterns.
decom_pattern -> decomp split words reasemb_patterns.
reasemb_patterns -> reasemb_pattern  reasemb_patterns.
reasemb_pattern -> reasemb split goto name.
reasemb_pattern -> reasemb split words. 
name -> word.

Left 500 script.
Right 300 words.

