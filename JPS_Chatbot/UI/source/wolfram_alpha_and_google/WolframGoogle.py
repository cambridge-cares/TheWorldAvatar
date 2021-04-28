# Our wolfram alpha user id is G5WRTA-882W6ATLGU
import json
from pprint import pprint
import wolframalpha
import requests

from selenium import webdriver
from bs4 import BeautifulSoup
# ========================== comment ========================
# The Google official api does a decent job, however, it does not return the KG result
# As a result, we tried to directly scrape the google result page ...
# To conclude, the direct scraping work fine.


class WolframGoogle:
    def __init__(self):
        # ================= setup wolfram alpha ==============
        self.wolfram_app_id = 'G5WRTA-882W6ATLGU'
        self.wolframalpha_client = wolframalpha.Client(self.wolfram_app_id)
        self.object_template = {"head": {"vars": ["v"]}, "results": {"bindings": []}}

    def get_result_from_wolfram(self, question):
        question = question.replace('add_sign', ' + ')
        res = self.wolframalpha_client.query(question)
        bindings = ''
        counter = 0
        try:
            for pod in res.pods:
                counter = counter + 1
                for sub in pod.subpods:
                    try:
                        text = sub.plaintext
                        if text is None:
                            try:
                                text = sub.img
                            except:
                                print('The attempt to get image failed')
                    except:
                        print('the attempt to get text failed')

                    if counter <= 2 and (text is not None):
                        bindings = bindings + '<br/>' + text
        except KeyError:
            print('Wolfram alpha failed to provide an answer')
            return 'Wolfram alpha failed to provide an answer'
        except AttributeError:
            print('Wolfram alpha failed to provide an answer')
            return 'Wolfram alpha failed to provide an answer'
        except TypeError:
            print('Wolfram alpha failed to provide an answer')
            return 'Wolfram alpha failed to provide an answer'
        return bindings.replace('"', '')


if __name__ == '__main__':
    wf = WolframGoogle()
    r = wf.get_result_from_wolfram('stability of FMOC amide + zinc')
    print(r)
