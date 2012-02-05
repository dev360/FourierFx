#coding=utf-8
from django.conf.urls.defaults import *

from tastypie.api import Api

from api.resources import SymbolResource, QuotesResource

from api import views

v1_api = Api(api_name='1.0')

v1_api.register(SymbolResource())       # /symbols/
v1_api.register(QuotesResource())       # /symbols/EURUSD/


urlpatterns = patterns('',
    (r'^api/', include(v1_api.urls)),
)

