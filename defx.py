#!/usr/bin/env python3.6
"""
Copyright 2017 Vehbi Sinan Tunalioglu <vst@vsthost.com>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

import base64
import datetime
import json
import sys
from decimal import Decimal
from http.client import HTTPResponse
from typing import Dict, Set, Tuple, Any, Type, Optional, Union, List
from urllib.error import URLError, HTTPError
from urllib.parse import urlencode, urlparse
from urllib.request import urlopen, Request

import logging
from collections import OrderedDict


#: Defines the library version.
Version = "0.0.2.dev0"


#: Defines the module logger.
Logger = logging.getLogger(__name__)


#: Defines all supported currencies.
SupportedCurrencies = {
    "AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG", "AZN",
    "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", "BOB", "BRL",
    "BSD", "BTC", "BTN", "BWP", "BYN", "BYR", "BZD", "CAD", "CDF", "CHF",
    "CLF", "CLP", "CNH", "CNY", "COP", "CRC", "CUC", "CUP", "CVE", "CZK",
    "DJF", "DKK", "DOP", "DZD", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP",
    "GBP", "GEL", "GGP", "GHS", "GIP", "GMD", "GNF", "GTQ", "GYD", "HKD",
    "HNL", "HRK", "HTG", "HUF", "IDR", "ILS", "IMP", "INR", "IQD", "IRR",
    "ISK", "JEP", "JMD", "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW",
    "KRW", "KWD", "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL", "LYD",
    "MAD", "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRO", "MUR", "MVR",
    "MWK", "MXN", "MYR", "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD",
    "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON",
    "RSD", "RUB", "RWF", "SAR", "SBD", "SCR", "SDG", "SEK", "SGD", "SHP",
    "SLL", "SOS", "SRD", "SSP", "STD", "SVC", "SYP", "SZL", "THB", "TJS",
    "TMT", "TND", "TOP", "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD",
    "UYU", "UZS", "VEF", "VND", "VUV", "WST", "XAF", "XAG", "XAU", "XCD",
    "XDR", "XOF", "XPD", "XPF", "XPT", "YER", "ZAR", "ZMK", "ZMW", "ZWL",
}


#: Defines the base currency.
BaseCurrency = "USD"


#: Defines the quanization template.
Quantizer = Decimal("0.00000001")

#: Defines the currency type.
CCY = str

#: Defines the type of the currency pair.
Pair = Tuple[CCY, CCY]

#: Defines the type of the date.
Date = datetime.date

#: Defines the type of the rates.
Rate = Decimal

#: Defines the type of partial rates for an arbitrary base currency on an arbitrary date.
PartialRates = Dict[CCY, Rate]

#: Defines the type of rates on an arbitrary date.
Rates = Dict[Pair, Rate]

#: Defines the type of daily rates.
DailyRates = Dict[Date, Dict[Pair, Rate]]


class DecimalEncoder(json.JSONEncoder):
    """
    Extends the JSON encoder for encoding `Decimal`s.

    Note: Adopted from http://stackoverflow.com/questions/4019856/decimal-to-json
    """

    def default(self, obj):
        if isinstance(obj, Decimal):
            return str(obj)
        return json.JSONEncoder.default(self, obj)


def rated(value: Union[None, Decimal, float, str]) -> Optional[Rate]:
    """
    Converts a value to a proper rate value.
    """
    ## Do we have a value?
    if value is None or value == "":
        ## Nope, return None
        return None

    ## Yep, make it decimal and quantize properly
    return Decimal(value).quantize(Quantizer)


class UserException(Exception):
    """
    Defines a user exception model.
    """
    pass


class APIClient:
    """
    Defines an API client.
    """

    class APIException(UserException):
        """
        Defines an API exception model.
        """
        pass

    #: Defines the default base API endpoint URL.
    baseurl = "https://openexchangerates.org/api"

    def __init__(self, apikey: str) -> None:
        """
        Initializes an API client.
        """
        self.__apikey = apikey

    def get(self, uri, params) -> HTTPResponse:
        """
        Returns the HTTP response for the given uri and params.
        """
        ## Get the query string parameters:
        qsparams = urlencode(params)

        ## Construct the URL:
        url = f"{self.baseurl}/{uri}?app_id={self.__apikey}&{qsparams}"

        ## Log it:
        Logger.debug(f"Reaching {url}")

        ## Open and return:
        return urlopen(url)  # type: ignore

    def __call__(self, uri: str, **kwargs) -> Dict[str, Any]:
        """
        Calls the remote endpoint and returns the result.
        """
        ## Attempt to make the request and retrieve the response.
        try:
            ## Attempt to open the url:
            with self.get(uri, kwargs) as response:
                ## Check the status code:
                if response.status != 200:
                    ## Something is wrong, raise Exception:
                    raise APIClient.APIException(f"Can not retrieve response from API: {response.status} {response.reason}")

                ## Get the response content:
                content = json.load(response)  # type: ignore

                ## Done, return the response:
                return content
        except HTTPError as exc:
            raise APIClient.APIException(f"Can not retrieve response from API: {exc.code} {exc.reason} {exc.read()}")
        except URLError as exc:
            raise APIClient.APIException(f"Can not connect to API: {exc}")
        except Exception as exc:
            raise APIClient.APIException(f"Unknown exception occured during API request: {exc}")


class TaskType:
    """
    Defines a task type.
    """

    #: Defines an internal registry of registered task types.
    _registry = {}  # type: Dict[str, Type[TaskType]]

    def __init_subclass__(cls, **kwargs):
        """
        Registeres the concrete task type class.
        """
        ## Call the super.
        super().__init_subclass__(**kwargs)

        ## Add to the registry:
        cls._registry[cls.code] = cls

    def __init__(self, client: APIClient) -> None:
        """
        Keep the client.
        """
        self._client = client

    @property
    def code(self) -> CCY:
        """
        Returns the code of the task type.
        """
        raise NotImplementedError

    def __call__(self, base: CCY, others: Set[CCY], **kwargs) -> DailyRates:
        """
        Carries on the task.
        """
        raise NotImplementedError

    @classmethod
    def of(cls, code: str) -> Type["TaskType"]:
        """
        Returns the task type by the given code.
        """
        return cls._registry.get(code)

    @classmethod
    def all(cls) -> Set[Type["TaskType"]]:
        """
        Returns available choices for task types, in particular themselves as classes.
        """
        return set(cls._registry.values())

    @classmethod
    def choices(cls) -> Set[CCY]:
        """
        Returns available choices for task types, in particular their codes.
        """
        return set(cls._registry.keys())

    @classmethod
    def pairup(cls, base: CCY, others: Set[CCY]) -> Set[Pair]:
        """
        Creates a set of FX pairs (tuples of CCY codes) of interest.
        """
        return {(base, c) for c in others if c != base}

    @classmethod
    def permute(cls, codes: Set[CCY]) -> Set[Pair]:
        """
        Creates permutation of all currency pair codes for the given codes.
        """
        return {(c1, c2) for c1 in codes for c2 in codes if not c1 == c2}

    @classmethod
    def getrate(cls, ccy1: CCY, ccy2: CCY, base: CCY, lookup: Rates) -> Rate:
        """
        Finds or computes the rate for the given two currencies and an FX rate database.
        """
        try:
            return rated(lookup[(ccy1, ccy2)])
        except KeyError:
            try:
                return rated(Decimal("1") / lookup[(ccy2, ccy1)])
            except KeyError:
                return rated(lookup[(base, ccy2)] / lookup[(base, ccy1)])

    @classmethod
    def getrates(cls, base: CCY, adict: Dict[CCY, float]) -> Rates:
        """
        Converts the API response fragment for rates to a proper rate lookup table.
        """
        return {(base, other): rated(rate) for other, rate in adict.items()}

    @classmethod
    def build(cls, base: CCY, rates: Rates) -> Rates:
        """
        Builds an `FX Pair` - `FX Rate` database for an arbitrary date.
        """
        ## Get all currencies involved except the base currency:
        currencies = {c for pair in rates.keys() for c in pair}

        ## Get pairs as all permutations of currencies:
        pairs = cls.permute(currencies)

        ## Get permutations, compute crosses and return:
        return {(ccy1, ccy2): cls.getrate(ccy1, ccy2, base, rates) for ccy1, ccy2 in pairs}

    @classmethod
    def flatten(cls, drates: DailyRates) -> List[Tuple[str, str, float]]:
        """
        Flattens the daily rates into a list of (date, Pair, rate) tuples.
        """
        return [(str(d), f"{p[0]}{p[1]}", float(r)) for d, rs in drates.items() for p, r in rs.items()]

    @classmethod
    def csvify(cls, drates: DailyRates, fdate: str="date", fpair: str="pair", frate: str="rate") -> str:
        """
        Returns a CSV representation of daily rates.
        """
        ## Define escape function:
        escape = lambda x: "," in x and "\"{}\"".format(x.replace("\"", "\\\"")) or x

        ## Escae fields:
        fdate = escape(fdate)
        fpair = escape(fpair)
        frate = escape(frate)

        ## Construct end
        return f"{fdate},{fpair},{frate}\n" + "\n".join([f"{d},{p},{r}" for d, p, r in cls.flatten(drates)]) + "\n"

    @classmethod
    def jsonify(cls, drates: DailyRates, fdate: str="date", fpair: str="pair", frate: str="rate") -> str:
        """
        Returns a json representation of daily rates.
        """
        return json.dumps([{fdate: d, fpair: p, frate: r} for d, p, r in cls.flatten(drates)])

    @classmethod
    def callback(cls, url) -> Tuple[str, Optional[Tuple[str, Optional[str]]]]:
        """
        Parses the callback and returns a tuple of url and authentication credentials if any.
        """
        ## Parse the url:
        parsed = urlparse(url)

        ## Get parts of the url:
        scheme = parsed.scheme
        hostname = parsed.hostname
        hostport = parsed.port and f":{parsed.port}" or ""
        hostpath = parsed.path
        hostargs = parsed.query and f"?{parsed.query}" or ""

        ## Re-construct URL:
        reurl = f"{scheme}://{hostname}{hostport}{hostpath}{hostargs}"

        ## Construct HTTP Basic auth credentials:
        credentials = parsed.username and (parsed.username, parsed.password)

        ## Return:
        return reurl, credentials

    @classmethod
    def handover(cls, jsondata: str, url: str, auth: Optional[Tuple[str, str]]) -> None:
        """
        Handsover the data to url using credentials by posting as a JSON.
        """
        ## Create the request:
        request = Request(url, data=jsondata.encode("utf-8"), headers={"Content-Type": "application/json"})

        ## Add authentication header if required:
        if auth:
            ## Get the token:
            token = base64.b64encode(f"{auth[0]}:{auth[1]}".encode("utf-8"))

            ## Add header:
            request.add_header("Authorization", f"Basic {token.decode('utf-8')}")

        ## Send:
        with urlopen(request) as response:
            assert response.status == 200  # type: ignore


class Latest(TaskType):
    """
    Provides a task type to retrieve latest FX rates.
    """

    #: Defines the code of the task type.
    code = "latest"

    def __call__(self, base: CCY, others: Set[CCY], **kwargs) -> DailyRates:
        """
        Carries on the task.
        """
        ## Get the API response:
        ## TODO: Note that symbols parameters is only for paid accounts.
        ## response = self._client("latest.json", base=base, symbols=",".join(others))
        response = self._client("latest.json", base=base)

        ## Get the date/time:
        date = datetime.datetime.fromtimestamp(response.get("timestamp")).date()

        ## Get rates:
        ## The necessity of this operation is as per the above TODO.
        rates = self.getrates(base, {c: v for c, v in response.get("rates", {}).items() if c in others})

        ## Build database and return:
        return OrderedDict([(date, self.build(base, rates))])


class Historical(TaskType):
    """
    Provides a task type to retrieve historical FX rates.
    """

    #: Defines the code of the task type.
    code = "historical"

    def __call__(self, base: CCY, others: Set[CCY], start: datetime.date, end: datetime.date, **kwargs) -> DailyRates:
        """
        Carries on the task.
        """
        ## Define the return value:
        retval = OrderedDict([])

        ## Start < end:
        assert start <= end

        ## Get the number of days:
        daycount = (end - start).days + 1

        for date in [(start + datetime.timedelta(days=i)) for i in range(0, daycount)]:
            ## Get the API response:
            ## TODO: Note that symbols parameters is only for paid accounts.
            ## response = self._client("latest.json", base=base, symbols=",".join(others))
            response = self._client(f"historical/{date}.json", base=base)

            ## Get rates:
            ## The necessity of this operation is as per the above TODO.
            rates = self.getrates(base, {c: v for c, v in response.get("rates", {}).items() if c in others})

            ## Build daily database and add to return value:
            retval.update(OrderedDict([(date, self.build(base, rates))]))

        ## Build database and return:
        return retval


if __name__ == "__main__":
    ## Load required libraries:
    from argparse import ArgumentParser, FileType

    ## Define the argument parser:
    parser = ArgumentParser(description="FXifies into your face")

    ## Add base currency argument:
    parser.add_argument("--base", default=BaseCurrency, choices=SupportedCurrencies)

    ## Add other currencies argument:
    parser.add_argument("--other", required=True, nargs="+", choices=SupportedCurrencies)

    ## Add base currency argument:
    parser.add_argument("--task", default=Latest.code, choices=TaskType.choices())

    ## Add api key argument:
    parser.add_argument("--apikey", required=True)

    ## Add api key argument:
    parser.add_argument("--output", default=sys.stdout, type=FileType("w"))

    ## Add debug parameter:
    parser.add_argument("--debug", action="store_true")

    ## Add format:
    parser.add_argument("--format", default="csv", choices={"csv", "json"})

    ## Add field renaming:
    parser.add_argument("--fields", default=["date", "symbol", "rate"], nargs=3)

    ## Add call backs:
    parser.add_argument("--callback", nargs="+", type=TaskType.callback)

    ## Add start date:
    parser.add_argument("--start", type=lambda x: datetime.datetime.strptime(x, "%Y-%m-%d").date())

    ## Add end date:
    parser.add_argument("--end", type=lambda x: datetime.datetime.strptime(x, "%Y-%m-%d").date())

    ## Add call backs
    parser.add_argument("--form", default=None)

    ## Read in the arguments:
    args = parser.parse_args()

    ## Define the logging format:
    logformat = "[%(asctime)-15s] %(message)s"

    ## Configure logging:
    logging.basicConfig(level=logging.DEBUG if args.debug else logging.INFO, format=logformat)

    ## Log arguments:
    Logger.debug(f"Version  : {Version}")
    Logger.debug("Arguments:")
    for key, value in args._get_kwargs():
        Logger.debug(f"    {key:10s} = {value}")

    ## Do it:
    if args.format == "csv" and args.callback:
        sys.stderr.write("Only JSON format is supported for callbacks. Exiting...\n")
        sys.exit(1)

    ## Get the API client:
    client = APIClient(args.apikey)

    ## Get the task type:
    tasktype = TaskType.of(args.task)

    ## Create the task:
    task = tasktype(client)

    ## Get the encoding function:
    if args.format == "csv":
        encoder = tasktype.csvify
    else:
        encoder = tasktype.jsonify

    ## Run the task, get the result and encode it into data:
    data = encoder(task(args.base, args.other, start=args.start, end=args.end), *args.fields)

    ## Write the data to the output file:
    with args.output as output:
        Logger.debug("Writing the data to output...")
        output.write(data)

    ## If we have any callbacks, handover the data:
    for url, credentials in (args.callback or []):
        ## Log it
        Logger.debug(f"Handing over the data to {url}")

        ## Do it:
        tasktype.handover(data, url, credentials)
