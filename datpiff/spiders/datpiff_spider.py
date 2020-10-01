from scrapy import Spider
from datpiff.items import DatpiffItem
from scrapy import Request
import re

class datpiff_spider(Spider):
    name = 'datpiff_spider'
    allowed_urls = ['https://www.datpiff.com/']
    start_urls = ['https://www.datpiff.com/mixtapes/2xplatinum/1']

    def parse(self,response):
        # Get urls for page one of each award level
        award_urls = ['https://www.datpiff.com' + x + '/1' for x in response.xpath('//div[@class="filterButtons"]//@href').extract()]
        # Iterate through award levels
        for award_page in award_urls:
            yield Request(url=award_page, callback=self.parse_pages)

    def parse_pages(self, response):
        #Get current page and next page for iteration
        current_page = int(response.xpath('//div[@class="pagination"]//a[@class="active"]/text()').extract_first())
        #current_page = int(re.split('/', response.xpath('//div[@class="pagination"]//a[@class="active"]/@href').extract_first())[-1])        
        #current_page = int(str.split(response.request.url,'/')[-1])
        next_page = int(str.split(response.xpath('//div[@class="pagination"]/a[@class="next"]/@href').extract_first(),'/')[-1])
        next_page_url = 'https://www.datpiff.com' + response.xpath('//div[@class="pagination"]/a[@class="next"]/@href').extract_first()
        # print('curr: ',current_page, 'next: ', next_page,'url: ', next_page_url)
        # get mixtape boxes
        mixtapes = response.xpath('//div[@id="leftColumnWide"]//div[@class="contentItemInner"]')
        # iterate through mixtapes pulling urls, artists, title, and banners
        for mixtape in mixtapes:
            mixtape_url = 'https://www.datpiff.com' + mixtape.xpath('.//div[@class="title"]//@href').extract_first()
            artist = mixtape.xpath('./div[@class="artist"]/text()').extract_first().strip()
            title = mixtape.xpath('./div[@class="title"]//text()').extract_first().strip()
            
            try:
                downloads = int(''.join(re.findall('\d+', mixtape.xpath('./div[text()="Downloads: "]/span/text()').extract_first())))
            except Exception as e:
                print(type(e),e)

            banners = mixtape.xpath('./a/div/@class').extract()

            if "banner sponsor" in banners:
                banner = "sponsored"
            elif "banner exclusive" in banners:
                banner = "exclusive"
            else:
                banner = ""
            if "banner official" in banners:
                official = 1
            else: 
                official = 0

            meta = {'artist' : artist,
                    'title' : title,
                    # 'exclusive' : exclusive,
                    # 'sponsored' : sponsored,
                    'banner' : banner,
                    'official' : official,
                    'downloads' : downloads}
            yield Request(url=mixtape_url, callback=self.parse_mixtape_page, meta=meta)
        if current_page < next_page:
            yield Request(url=next_page_url,callback=self.parse_pages)

    # Get information from individual mixtape pages            
    def parse_mixtape_page(self, response):
        info = response.xpath('//div[@class="module1"]')
        downloads = response.meta['downloads']
        try:
            host = str.strip(info.xpath('.//li[@class="dj"]/text()').extract_first())   
        except Exception as e:
            print(type(e),e)
            host = None

        try:
            views = ''.join(re.findall('\d+', info.xpath('.//li[@class="listens"]/text()').extract_first()))
        except Exception as e:
            print(type(e),e)
            views = None

        try:
            release_date = info.xpath('//div[@class = "left"]//span/text()').extract_first()
        except Exception as e:
            print(type(e),e)
            release_date = None

        try:
            added_by = info.xpath('//div[@class = "left"]//a/text()').extract_first()
        except Exception as e:
            print(type(e),e)
            added_by = None

        try:
            description = str.strip(info.xpath('.//div[@class="description"]//text()').extract_first())
        except Exception as e:
            print(type(e),e)
            description = ''
        
        try:
            stat = [re.split('\-|\.',value)[-2] for value in info.xpath('.//div[@class="downloads right"]//li/img/@src').extract()]
            count = [str.strip(x) for x in info.xpath('.//div[@class="downloads right"]//li/text()').extract()]
            stats = {stat[x]:count[x] for x in range(0,len(stat))}
            # listens = stats['listens']
            listens = int(''.join(re.findall('\d+', stats['listens'])))
        except Exception as e:
            print(type(e),e)
            listens = None

        # If scraper is unable to get award level, impute by calculating based on downloads
        award = response.xpath('//ul[@class="awards "]//span/text()').extract_first()
        if award is None:
            if downloads >= 2e6: award = 'Double Diamond'
            elif downloads >=1e6: award = 'Diamond'
            elif downloads >=500000: award = 'Double Platinum'
            elif downloads >=250000: award = 'Platinum'
            elif downloads >=100000: award = 'Gold'
            elif downloads >=50000: award = 'Silver'
            elif downloads >=25000: award = 'Bronze'

        try:
            rating_count = int(''.join(re.findall('\d+',str.split(response.xpath('//div[@class="ratingBar"]/@title').extract_first())[0])))
        except Exception as e:
            print(type(e),e)
            rating_count = None
        
        try:
            rating_score = int(response.xpath('//div[@class="ratingBar"]/@data-rating-percent').extract_first())    
        except Exception as e:
            print(type(e),e)
            rating_score = int(response.xpath('//div[@class="ratingBar"]/@data-rating-percent').extract_first())

        try:
            tracks = len(response.xpath('//span[@class="tracknumber"]/text()').extract())
        except Exception as e:
            print(type(e),e)
            tracks = len(response.xpath('//span[@class="tracknumber"]/text()').extract())
        
        item = DatpiffItem()
        item['title'] = response.meta['title']
        item['artist'] = response.meta['artist']
        item['official'] = response.meta['official']
        item['downloads'] = response.meta['downloads']
        item['award'] = award
        item['host'] = host
        item['views'] = views
        item['release_date'] = release_date
        item['added_by'] = added_by
        item['description'] = description
        item['listens'] = listens
        item['tracks'] = tracks
        item['rating_count'] = rating_count
        item['rating_score'] = rating_score
        item['banner'] = response.meta['banner']
        yield item  