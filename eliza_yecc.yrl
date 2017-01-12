Nonterminals
script initial_sentence final_sentence quit_sentence pre_replace post_replace synon_words keyword decom_pattern decom_patterns reasemb_patterns reasemb_pattern words priority name.


Terminals
word split initial final quit pre post synon key decomp reasemb number goto.

Rootsymbol script.

script -> initial_sentence split script. 
script -> final_sentence split script.
script -> quit_sentence split script.
script -> pre_replace split script.
script -> post_replace split script.
script -> synon_words split script.
script -> keyword split script.
initial_sentence -> initial split words.
words -> word words.
final_sentence -> final split words.
quit_sentence -> quit split words.
pre_replace -> pre split word word.
post_replace -> post split word word.
synon_words -> synon split word words.
keyword -> key split name priority split decom_patterns.
name -> word.
priority -> '$empty'.
priority -> number.
decom_patterns -> decom_pattern split decom_patterns.
decom_pattern -> decomp split words split reasemb_patterns.
reasemb_patterns -> reasemb_pattern  split reasemb_patterns.
reasemb_pattern -> reasemb split goto name.
reasemb_pattern -> reasemb split words. 
