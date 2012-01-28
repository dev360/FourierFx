import logging
import simplejson

from django.conf.urls.defaults import *
from django.core.exceptions import ObjectDoesNotExist, MultipleObjectsReturned
from django.core.urlresolvers import reverse

from tastypie import fields
from tastypie.http import HttpBadRequest, HttpForbidden, HttpNotFound
from tastypie.resources import ModelResource, ALL, ALL_WITH_RELATIONS
from tastypie.utils import trailing_slash


class QuotesResource(ModelResource):
    """ Quotes resource """


    def obj_get_list(self, request=None, **kwargs):
        # Rewrite some kwargs..
        
        return []

    class Meta:
        resource_name = 'quotes'
        detail_allowed_methods = ['get',]
        list_allowed_methods = ['get',]

