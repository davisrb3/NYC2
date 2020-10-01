# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy

class DatpiffItem(scrapy.Item):
    # define the fields for your item here like:
    title = scrapy.Field()
    artist = scrapy.Field()
    release_date = scrapy.Field()
    host = scrapy.Field()
    award = scrapy.Field()
    views = scrapy.Field()
    downloads = scrapy.Field()
    listens = scrapy.Field()
    tracks = scrapy.Field()
    official = scrapy.Field()
    #instant_dl = scrapy.Field()
    #comments = scrapy.Field()
    #fb_likes = scrapy.Field()
    description = scrapy.Field()
    added_by = scrapy.Field()
    rating_count = scrapy.Field()
    rating_score = scrapy.Field()
    #exclusive = scrapy.Field()
    #sponsored = scrapy.Field()
    banner = scrapy.Field()