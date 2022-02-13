import sys

from theta.avro import Decoder, Encoder

from cat_everything.everything import Everything

def main():
    decoder = Decoder(sys.stdin.buffer)
    records = decoder.array(lambda: Everything.decode_avro(decoder))

    encoder = Encoder(sys.stdout.buffer)
    encoder.array(records, lambda everything: everything.encode_avro(encoder))
    sys.stdout.flush()
    
