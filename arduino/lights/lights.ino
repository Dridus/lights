#include <FastLED.h>

#define DATA_PIN 6
#define NUM_LEDS 100
#define MAX_LERPS 64
#define COLOR_ORDER RGB
//#define REPORT_INTERVAL 1000
#define REPORT_INTERVAL 5000
#define FPS 60

#define FRAME_MICROS (1000000/FPS)

// croak codes
#define HELLO_WORLD 1
#define I_JUST_DONT_UNDERSTAND_YOU 2
#define SORRY_WERE_FULL 3

// lerp responses
#define RESYNC   1
#define NEW_LERP 2
#define NO_LERPS 3

// lerp states
#define STATE_EMPTY     0
#define STATE_UNSTARTED 1
#define STATE_RUNNING   2

typedef struct {
  uint8_t       state;
  uint8_t       led;
  CRGB          start_color;
  unsigned long start_clock;
  CRGB          end_color;
  uint16_t      duration;
} lerp_t;

CRGB leds[NUM_LEDS];
lerp_t lerps[MAX_LERPS];

uint8_t next_lerp_index;
uint8_t num_lerps;
unsigned long last_ingest = 0;

void setup() {
  memset(leds, 0, sizeof(CRGB)*NUM_LEDS);
  memset(lerps, 0, sizeof(lerp_t)*MAX_LERPS);
  next_lerp_index = 0;
  num_lerps = 0;
  FastLED.addLeds<WS2811, DATA_PIN, COLOR_ORDER>(leds, NUM_LEDS);
  FastLED.clear(true);
  Serial.begin(115200);
  croak(HELLO_WORLD);
}

void loop() {
  unsigned long frame_start = micros();
  unsigned long frame_start_ms = frame_start / 1000L;
  FastLED.show();
  unsigned long since_last_ingest
    = (frame_start < last_ingest)
    ? (frame_start + ((~0L)-last_ingest))
    : (frame_start - last_ingest);
  if (since_last_ingest > 500000) {   
    ingest(frame_start_ms);
  }
  update_lerps(frame_start_ms);

  unsigned long frame_end = micros();
  unsigned long frame_duration
    = (frame_end < frame_start)
    ? (frame_end + ((~0L)-frame_start))
    : (frame_end - frame_start);
  if (frame_duration < FRAME_MICROS) {
    delay(frame_duration / 1000);
    delayMicroseconds(frame_duration % 1000);
  }
}

void croak(int code) {  
  Serial.print("croaking with ");
  Serial.println(code);
  memset(leds, 0, sizeof(CRGB)*NUM_LEDS);
  for (int repetition = 0; repetition < 3; ++repetition) {
    for (int blink = 0; blink < code; ++blink) {
      leds[0] = CRGB(128,0,0);
      leds[NUM_LEDS-1] = CRGB(128,0,0);
      FastLED.show();
      delay(100);
      leds[0] = CRGB(0,0,0);
      leds[NUM_LEDS-1] = CRGB(0,0,0);
      FastLED.show();
      delay(50);
    }
    delay(600);
  }
  
  while (Serial.available() > 0) (void)Serial.read();
}

void ingest(unsigned long now) {
  if (num_lerps == MAX_LERPS) {
    return;
  }
  
  while (1) {
    Serial.print(now); Serial.print(' ');
    if (num_lerps == MAX_LERPS) {
      Serial.print("x ");
    }
    
    Serial.println(num_lerps);
    
    Serial.setTimeout(1000);
    String line = Serial.readStringUntil('\n');
    int pos = line.indexOf(' ');
    if (pos < 0) { pos = line.length(); }
    long res = line.substring(0, pos).toInt();
    switch (res) {
      case 0:
        if (line.length() > 0) {
          Serial.print("failed to parse command from: ");
          Serial.println(line);
        }
        
        memset(leds, 8, sizeof(CRGB)*NUM_LEDS);
        for (uint8_t i = 0; i < random8() % 3; ++i) {
          uint8_t c = 16 + (random8() % 8 - 4);
          leds[random8() % NUM_LEDS] = CRGB(c,c,c);
        }
        FastLED.show();
        return;
      
      case RESYNC:
        Serial.println("resyncing");
        memset(leds, 0, sizeof(CRGB) * NUM_LEDS);
        memset(lerps, 0, sizeof(lerp_t) * MAX_LERPS);
        num_lerps = 0;
        next_lerp_index = 0;
        break;
        
      case NEW_LERP:
        {
          int pos2 = line.indexOf(' ', pos+1);
          int pos3 = line.indexOf(' ', pos2+1);
          int pos4 = line.indexOf(' ', pos3+1);
          int pos5 = line.indexOf(' ', pos4+1);
          int pos6 = line.indexOf(' ', pos5+1);

          if (pos2 < 0 || pos3 < 0 || pos4 < 0 || pos5 < 0 || pos6 < 0) {
            Serial.print("malformed lerp: ");
            Serial.println(line);
            return;
          }
          
          int i = 0;
          while (i < MAX_LERPS) {
            next_lerp_index %= MAX_LERPS;
            if (lerps[next_lerp_index].state == STATE_EMPTY) break;
            ++next_lerp_index;
            ++i;
          }

          if (i == MAX_LERPS) {
            croak(SORRY_WERE_FULL);
            next_lerp_index = 0;
            return;
          }
          
          lerp_t* lerp = lerps + next_lerp_index;
          uint8_t r,g,b;
          
          lerp->led = line.substring(pos+1,pos2).toInt();
          lerp->start_clock = line.substring(pos2+1,pos3).toInt();
          lerp->duration = line.substring(pos3+1,pos4).toInt();
          r = line.substring(pos4+1,pos5).toInt();
          g = line.substring(pos5+1,pos6).toInt();
          b = line.substring(pos6+1,line.length()).toInt();
          lerp->start_color = CRGB(0,0,0);
          lerp->end_color = CRGB(r,g,b);
          lerp->state = STATE_UNSTARTED;
          ++num_lerps;
          ++next_lerp_index;
        }
        break;

      case NO_LERPS:
        return;
        
      default:
        croak(I_JUST_DONT_UNDERSTAND_YOU);
    }
  }
}

void update_lerps(unsigned long now) {
  for (int lerp_n = 0; lerp_n < MAX_LERPS; ++lerp_n) {
    lerp_t* lerp = lerps + lerp_n;
    if (lerp->state != STATE_EMPTY) {
      CRGB* led = lerp->led == 0 ? NULL : (leds + (lerp->led - 1));
      update_lerp(now, led, lerp); // lerp
    }
  }
}

void update_lerp(unsigned long now, CRGB* led, lerp_t* lerp) {
  //Serial.print("l"); Serial.print(lerp->led); Serial.print("d"); Serial.print(lerp->duration); Serial.print("n"); Serial.print(now); Serial.print("s"); Serial.println(lerp->start_clock); 
  if (now < lerp->start_clock) return;
  if (now >= lerp->start_clock + lerp->duration) {
    if (led != NULL) {
      *led = lerp->end_color;
    } else {
      for (int i = 0; i < NUM_LEDS; ++i) {
        leds[i] = lerp->end_color;
      }
    }
    --num_lerps;
    memset(lerp, 0, sizeof(lerp_t));
  } else {
    if (lerp->state == STATE_UNSTARTED) {
      lerp->start_color = led != NULL ? *led : leds[0];
      lerp->state = STATE_RUNNING;
    }
    unsigned long elapsed = now - lerp->start_clock;
    unsigned long duration = lerp->duration;
    CRGB color = lerp->start_color.lerp8(lerp->end_color, (fract8)((elapsed << 8) / duration));
    if (led != NULL) {
      *led = color;
    } else {
      for (int i = 0; i < NUM_LEDS; ++i) {
        leds[i] = color;
      }
    }
  }
}

