from requests import get
import Quandl

baseurl = 'http://api.worldbank.org/countries/all/indicators/'

indicators = ['SP.DYN.SMAM.FE']

fullurl = ''.join((baseurl, indicators[0]))

payload = {'format': 'json', 'date': '2007:2012', 'per_page': '5000'}

response = get(fullurl, params=payload)

j = response.json()[1]

a = []
for country in j:
    if country['value']:
        a.append(country)

def country(name, r=a):
    '''
    Pick out one country
    '''
    out = []
    for country in r:
        if country['country']['value'] == name:
            out.append(country)
    return out


us = country('United States')

# Try out the Quandl solution
# Bummed they won't return what I want- a list of this indicator 
# for all countries!
#germany = Quandl.get("WORLDBANK/DEU_SP_DYN_SMAM_FE")
