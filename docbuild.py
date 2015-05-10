"""
Library to Process README.md file.

"""
import prelude as p
import os

from prelude import Str as st
from prelude import Chain, Lazy, X
from prelude import entryPoint

import re

findTitle  = lambda text: re.findall("^## (.*)", text, re.M)
findTopics = lambda text: re.findall("^## (.*)", text, re.M)


makeLink = lambda topic, link: "[{}]({})".format(topic, link)

findReSpan =  lambda pattern, text: (
                        Chain(re.finditer(pattern, text, re.M))
                        .m(p.mcallm("span"))
                        .toList()
                        .value()
                        )

#-----------------------------------------------------------#




#@entryPoint
#def main():

text = open("README2.md").read()
topics = findTopics(text)

span = findReSpan("^## (.*)", text)

sections = p.zipl(p.column_nth(1, span), p.tail(p.column_nth(0, span)))



get_divisions = p.mapf(p.uncurry(p.slice(text)))

subtexts = get_divisions(p.Lazy.pairsl(p.column_nth(0, span)))

#sectext = p.mapl(p.uncurry(p.slice(text)), sections)


filenames =  p.mapl( 
    p.compose(
        st.replaceList(["/", ",", " "], "_"), 
        st.addSuffix(".md"),
    ), 
    topics
    )

links = p.compose(
    st.joinLines, 
    p.mapl(st.addPrefix("* ")),
    p.mapl(p.uncurry(makeLink),
    ) # End of composition
    )(p.zipl(topics, p.mapl(st.addPrefix("pages/"), filenames)))


#os.chdir("./pages")

#p.mapl(p.uncurry(p.writeFile), p.zipl(filenames, subtexts))

"""
mapl(p.uncurry(p.writeFile), zip(filenames, sectext)


>>> sections 
    [(8767, 11873),
 (11907, 18758),
 (18773, 31515),
 (31527, 70966),
 (70985, 73305),
 (73326, 75269),
 (75290, 79748),
 (79793, 108256),
 (108307, 118415),
 (118430, 142440),
 (142452, 149731),
 (149746, 151984)]



"""
