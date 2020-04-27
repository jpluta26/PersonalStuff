# pluta 5/1/17
# from upenn cs172 course
import re
import requests
import string
from bs4 import BeautifulSoup, SoupStrainer
import urllib2
from itertools import chain
import collections
import os, errno

''' 1. Generate Regular Expression for an email: 
        The name must begin with a letter and contain only letters, number,
        underscores and dots.
        The domain must be entirely letters, plus dots.
        The tld (top level domain) must be "com", "net" or "edu".
        There must be an '@' symbol between the name and the domain, and a dot
        between the domain and tld.
        Return each email as a tuple of (name, domain, tld)

    2. Take in a seed website. Your search will start at this website and 
        continue in a breadth-first manner up to a certain depth. That is,
        you will search each website for email addresses and links.
        Store each email that you find and check the pages linked
        from the website. If the links on the website do not start
        with your current seed, add this seed text to the beginning of 
        the link. When making requests to websites that give
        4xx or 5xx errors, skip these websites in your search. Your 
        implementation must not visit repeated links more than once.

    3. Write each of the email tuples found to a file called 'emails.txt'
    in sorted order, one per line

    A few tips:
        - Use the verify=False kwarg on request.get to make sure
        nothing goes wrong accessing https sites

        - Websites will only be tested for depths up to about 
        50 websites. In my solution, this depth starts to find
        weird fringe links on Penn pages that don't behave well.

        - Try http://www.upenn.edu/, https://www.math.upenn.edu/,
        https://www.seas.upenn.edu/ for some good results.

        - Do small testing for your scraper on this toy playground
        I set up: http://cis.upenn.edu/~cis192/html/files/lec
'''
# ------------------------------------------------------------------ #
def scrape(seed, max_sites=50):
# function to find all valid email addresses on a website
# input: seed, the website url
# max_sites: deprecated
#
# output: a list of valid emails
    
  
    # load the page and find all emails
    # return empty if link is not a page
    try:
        urllib2.urlopen(seed)
    except urllib2.URLError as err:
        return []
    except Exception:
        return []

    page = urllib2.urlopen(seed)
    dat = BeautifulSoup(page.read(), 'html.parser')
    emails = re.findall(r'User Friendly Budget', str(dat))
    
    # list to store email addresses
    lst = []



    # check each email for validity
    for email in emails:
        valid = True
        name = email.split('@')[0]
        domain = email.split('@')[1][:-4]
        tld = email.split('@')[1].split('.')[len(email.split('@')[1].split('.')) - 1]
        

        # name can include any letter, number, ., or _
        # first character must be a letter
        nset = set('_.0123456789') | set(string.ascii_lowercase) | set(string.ascii_uppercase)

        if any((c not in nset) for c in name):
            valid = False

        if name[0].isalpha() is False:
            valid = False

        # domain must be letters or .
        dset = set('.') | set(string.ascii_lowercase) | set(string.ascii_uppercase)

        if any((c not in dset) for c in domain):
            valid = False

        # tld must be com, net, edu
        if tld not in ("com", "net", "edu"):
            valid = False

        if valid:
            lst = lst + [(name, domain, tld)]

    # easier to read and sort this way, but could be done with tuples
    out_lst = []
    for i in lst:
        out_lst = out_lst + [str(i[0] + "@" + i[1] + "." + i[2])]

    return out_lst    
# ------------------------------------------------------------------ #



# ------------------------------------------------------------------ #
def getLinks( url, depth ):
# from: 
# http://stackoverflow.com/questions/20198934/how-to-get-all-links-from-website-using-beautiful-soup-python-recursively
# with modification
# search through links of a website recursively
# input: url, a seed URL to search
#   depth, the maximum number of links to search, so this doesnt loop infinitely
# output: a tuple of links
    # 5 is the max depth; user defined
    if depth == 5:
        return url
    else:

        # exception for links that arent pages
        try:
            urllib2.urlopen(url)
        except urllib2.URLError as err:
            return url
        except Exception:
            return url

        page = urllib2.urlopen(url)
        soup = BeautifulSoup(page.read(), "html.parser")
        newlink = soup.find('a')

        if newlink is None:
            return [url]
        else:
            return [url, getLinks(str(url + newlink['href']), depth + 1)]
# ------------------------------------------------------------------ #




# ------------------------------------------------------------------ #       
def main():
    # seed = "http://cis.upenn.edu/~cis192/html/files/lec"
    
    seed = "http://www.vernontwp.com/government/annual_budget/"
    print("working...")
    # delete emails.txt if it already exists from a previous run
   

    # open the inital page
    page = urllib2.urlopen(seed)
    s = BeautifulSoup(page.read(), "html.parser")
    links = s.find_all('a')

    # the list of pages to check; e.g., the seed plus all linked pages
    lst = [seed]
    
    # recursively build the list of links
    # this is a list of strings and lists (of strings)
    err = 0
    for link in links:
        try:
            link['href']
            lst = lst + [getLinks(   str(seed + "/" + link['href']),   0 )] 
        except KeyError:
            err += 1

    
    # list of emails addresses; initially this is a list of both strings and lists
    # needs to be flattened to just a list of strings
    emails = []
    for i in lst:
        if type(i) == type(str()):
            emails = emails + [scrape(i)]
        elif type(i) == type(list()):
            for j in i:
                emails = emails + [scrape(j)]

    # flatten list
    emails = [ item for sublist in emails for item in sublist ]

    # write to file. emails should be unique, sorted, and on separate lines
    outfile = open("emails.txt", 'w')
    outfile.write(str("\n".join(sorted(set(emails)))))
    outfile.close()
    print("done. emails in 'emails.txt'")

if __name__ == "__main__":
    main()
# ------------------------------------------------------------------ #
