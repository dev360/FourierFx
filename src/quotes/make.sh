#!/bin/bash
rm -rf bin/*
ghc -o bin/quote_publisher QuotePublisher.hs
ghc -o bin/quote_subscriber QuoteSubscriber.hs
