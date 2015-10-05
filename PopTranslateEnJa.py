# coding=UTF-8
import sublime, sublime_plugin
import urllib.request
import xml.etree.ElementTree as ET

class popTranslateEnglishIntoJapaneseCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        search_word = self.getSearchWord()
        if search_word is None:
            sublime.status_message("Nothing to search!")
            return

        item_id = self.getItemID(search_word)
        if item_id is None:
            sublime.status_message("No translation!")
            return

        text = self.getTranslatedText(item_id)
        if text is None:
            return

        text_arr = self.splitTranslatedText(text, '\t')
        self.view.show_popup_menu(text_arr, None)


    def getSearchWord(self):
        region = self.view.sel()[0]
        if region.empty():
            word = self.view.word(region)
            if not word.empty():
                return self.view.substr(word)
        else:
            return self.view.substr(region)


    def getXmlElementText(self, url, tag):
        print('url : ' + url)
        try:
            xml = urllib.request.urlopen(url)
        except urllib.error.HTTPError as e:
            print('error code : ' + str(e.code))
            print('error read : ' + str(e.read()))
            return

        print(xml)
        tree = ET.parse(xml)
        root = tree.getroot()
        element = root.find('.//{http://btonic.est.co.jp/NetDic/NetDicV09}' + tag)
        if element is None:
            return
        text = element.text
        print(text)
        return text


    def getItemID(self, search_word):
        head = 'http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite?Dic=EJdict&Word='
        end = '&Scope=HEADWORD&Match=EXACT&Merge=OR&Prof=XHTML&PageSize=20&PageIndex=0'
        url = head + search_word + end
        return self.getXmlElementText(url, 'ItemID')


    def getTranslatedText(self, item_id):
        head = 'http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite?Dic=EJdict&Item='
        end = '&Loc=&Prof=XHTML'
        url = head + item_id + end
        return self.getXmlElementText(url, 'Body/div/div')


    def splitTranslatedText(self, translated_text, split_word):
        return translated_text.split(split_word)
