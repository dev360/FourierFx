from datetime import datetime
from decimal import Decimal
import logging
import simplejson

from django.conf.urls.defaults import *
from django.core.exceptions import ObjectDoesNotExist, MultipleObjectsReturned
from django.core.urlresolvers import reverse

from tastypie import fields
from tastypie.http import HttpBadRequest, HttpForbidden, HttpNotFound
from tastypie.resources import Resource, ModelResource, ALL, ALL_WITH_RELATIONS
from tastypie.utils import trailing_slash

from quotes.utils.redisdb import RedisDB

class Symbol(object):
    """ Represents a quote symbol """

    def __init__(self, *args, **kwargs):
        self.__dict__ = kwargs

        if args:
            self.symbol = args[0]


class SymbolResource(Resource):
    """ Symbol resource """

    symbol = fields.CharField(attribute='symbol', null=True)

    def get_resource_uri(self, bundle, **kwargs):
        kwargs['pk'] = bundle.obj.symbol
        if self._meta.api_name is not None:
            kwargs['api_name'] = self._meta.api_name
        return reverse('api_quotes_detail', kwargs=kwargs)

    def base_urls(self):
        return []

    def override_urls(self):
        return [
            url(r"^(?P<resource_name>%s)%s$" % (self._meta.resource_name, trailing_slash()), self.wrap_view('dispatch_list'), name="api_symbols_list"),
        ]

    def obj_get_list(self, request=None, **kwargs):
        # Rewrite some kwargs..
        redis = RedisDB()
        symbols = [x for x in redis.smembers('symbols')]
        return [  Symbol(x) for x in symbols]

    class Meta:
        resource_name = 'symbols'
        list_allowed_methods = ['get',]


class Quote(object):
    """ Represents a qoute """

    def __init__(self, *args, **kwargs):

        self.type = args[0]

        self.symbol = kwargs.get('symbol')
        self.date = datetime.strptime(kwargs.get('date'), '%Y-%m-%dT%H:%M:%S.000Z')

        if self.type.lower() == 'ask':
            self.price = Decimal(str(kwargs.get('ask')))
            self.volume = Decimal(str(kwargs.get('ask_volume')))
        else:
            self.price = Decimal(str(kwargs.get('bid')))
            self.volume = Decimal(str(kwargs.get('bid_volume')))


    def __unicode__(self):
        return u'{0}'.format(self.symbol)


class QuotesResource(Resource):
    """ Quotes resource """

    date = fields.DateTimeField(attribute='date', null=True)
    symbol = fields.CharField(attribute='symbol', null=True)
    price = fields.DecimalField(attribute='price', null=True)
    volume = fields.DecimalField(attribute='volume', null=True)
    type = fields.CharField(attribute='type', null=True)

    def base_urls(self):
        return []

    def override_urls(self):
        return [
            url(r"^symbols/(?P<symbol>[^/]+)/(?P<resource_name>%s)%s$" % (self._meta.resource_name, trailing_slash()), self.wrap_view('dispatch_list'), name="api_quotes_list"),
        ]

    def obj_get_list(self, request=None, **kwargs):
        symbol = 'symbol_{0}'.format(kwargs.get('symbol').upper())
        redis = RedisDB()
        json = '[{0}]'.format(','.join(redis.lrange(symbol, 0, 1000)))
        
        quote_type = request.GET.get('type', 'ask')
        objs = [Quote(quote_type, **x) for x in simplejson.loads(json)]
        return objs

    class Meta:
        resource_name = 'quotes'
        list_allowed_methods = ['get',]
        filtering = {
            "type": ALL_WITH_RELATIONS
        }
        
