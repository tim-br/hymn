# Hymn 🎵

A Haskell library and command-line tool for generating MusicXML files through functional composition and music theory.

<!-- [![Build Status](https://img.shields.io/badge/build-passing-brightgreen)]()
[![License](https://img.shields.io/badge/license-BSD3-blue)]()
[![Haskell](https://img.shields.io/badge/language-Haskell-purple)]() -->

## Features

- **Type-safe music representation** with Haskell's strong type system
- **Automatic scale generation** with proper accidentals and octave handling
- **MusicXML output** compatible with notation software (MuseScore, Finale, Sibelius)
- **Modular architecture** with separate core theory and XML rendering libraries
- **Comprehensive testing** with unit, property-based, and golden file tests
- **Command-line interface** for quick music generation

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/tim-br/hymn
cd hymn

# Build with Stack
stack build

# Run the CLI
stack exec hymn -- generate
```

### Basic Usage

```bash
# Generate a major scale
stack exec hymn -- generate

# Show version
stack exec hymn -- version
```

This will create a `scale.musicxml` file containing an E major scale that you can open in any MusicXML-compatible notation software.

## Library Usage

### Generating Scales

```haskell
import Hymn.Types
import Hymn.Theory
import Hymn.Render

-- Generate D Major scale
let dMajorScale = generateMajorScale D 4
-- Result: [D4, E4, F#4, G4, A4, B4, C#5, D5]

-- Create a complete MusicXML score
let attributes = Attributes 4 (Just (Key 0 Nothing)) (Just (TimeSig 4 4)) (Just (Clef "G" 2))
    measure = Measure 1 (Just attributes) (map MNote dMajorScale)
    part = Part "P1" [measure]
    score = ScorePartwise "3.1" [ScorePart "P1" "D Major Scale"] [part]

-- Export to MusicXML
writeMusicXML "d-major.xml" score
```

### Creating Individual Notes

```haskell
-- Create a middle C quarter note
let middleC = Note 
      { pitch = Just (Pitch C Nothing 4)
      , duration = 4
      , voice = 1
      , noteType = Just Quarter
      , accidental = Nothing
      , rest = False
      , tieStart = False
      , tieStop = False
      }
```

### Working with Accidentals

```haskell
-- F# (F sharp)
let fSharp = Pitch F (Just 1) 4

-- Bb (B flat)  
let bFlat = Pitch B (Just (-1)) 4

-- The library automatically generates correct accidentals for scales
let fMajorScale = generateMajorScale F 4  -- Includes Bb automatically
```

## Architecture

Hymn is built with a modular architecture:

```
hymn/
├── hymn-core/           # Core music theory types and functions
│   ├── src/Hymn/
│   │   ├── Types.hs     # Data types for musical concepts
│   │   └── Theory.hs    # Scale generation and music theory
│   └── hymn-core.cabal
├── hymn-xml/            # MusicXML rendering
│   ├── src/Hymn/
│   │   └── Render.hs    # MusicXML generation
│   └── hymn-xml.cabal
├── app/                 # Command-line interface
│   └── Hymn/
│       ├── Main.hs
│       └── CLI.hs
├── test/                # Comprehensive test suite
│   ├── Spec.hs         # Unit tests
│   ├── Properties.hs   # Property-based tests
│   ├── Integration.hs  # Integration tests
│   └── Golden.hs       # Golden file regression tests
└── hymn.cabal          # Main executable
```

### Core Types

```haskell
-- Pitch representation
data Pitch = Pitch
  { step :: PitchStep      -- A, B, C, D, E, F, G
  , alter :: Maybe Int     -- Sharps (+1) or flats (-1)
  , octave :: Int          -- Octave number
  }

-- Note with timing and voice information
data Note = Note
  { pitch :: Maybe Pitch
  , duration :: Int
  , voice :: Int
  , noteType :: Maybe NoteType
  , accidental :: Maybe Accidental
  , rest :: Bool
  , tieStart :: Bool
  , tieStop :: Bool
  }
```

## Music Theory Features

### Scale Generation

Currently supports major scales with:
- **Automatic accidental calculation** based on key signature
- **Proper octave handling** when crossing octave boundaries
- **All 12 major scales** (C, G, D, A, E, B, F#, Db, Ab, Eb, Bb, F)

```haskell
-- Examples of different scales
let cMajor = generateMajorScale C 4  -- No accidentals
let gMajor = generateMajorScale G 4  -- F#
let fMajor = generateMajorScale F 4  -- Bb
let dMajor = generateMajorScale D 4  -- F#, C#
```

### Extensible Design

The library is designed to be easily extended with:
- Minor scales, modes, and exotic scales
- Chord generation and progressions
- Rhythm patterns and time signatures
- Advanced harmony and counterpoint

## Testing

Hymn includes comprehensive testing:

```bash
# Run all tests
stack test

# Run specific test categories
stack test --test-arguments="--match=\"Music Theory\""
stack test --test-arguments="--match=\"XML Rendering\""

# Run with coverage
stack test --coverage
```

### Test Categories

1. **Unit Tests** - Test individual functions and components
2. **Property Tests** - Verify mathematical properties (e.g., scales always have 8 notes)
3. **Integration Tests** - Test complete workflows and file generation
4. **Golden Tests** - Regression testing against reference XML output

## Examples

### Generate All Major Scales

```haskell
import Hymn.Types
import Hymn.Theory
import Hymn.Render

generateAllMajorScales :: IO ()
generateAllMajorScales = do
  let scales = [C, D, E, F, G, A, B]
  mapM_ generateScaleFile scales
  where
    generateScaleFile step = do
      let scale = generateMajorScale step 4
          attributes = Attributes 4 (Just (Key 0 Nothing)) (Just (TimeSig 4 4)) (Just (Clef "G" 2))
          measure = Measure 1 (Just attributes) (map MNote scale)
          part = Part "P1" [measure]
          score = ScorePartwise "3.1" [ScorePart "P1" (show step ++ " Major")] [part]
          filename = show step ++ "-major.xml"
      writeMusicXML filename score
      putStrLn $ "Generated " ++ filename
```

### Create a Simple Melody

```haskell
createMelody :: [PitchStep] -> [Note]
createMelody steps = 
  map (\step -> Note (Just (Pitch step Nothing 4)) 4 1 (Just Quarter) Nothing False False False) steps

-- Twinkle Twinkle Little Star (first phrase)
let melody = createMelody [C, C, G, G, A, A, G]
```

## Compatibility

### Tested with:
- **Noteflight** (web-based) ✅
- **Verovio** (with proper MusicXML structure) ✅

### MusicXML Standards:
- MusicXML 3.1 compliant
- Proper UTF-8 encoding
- Valid XML structure with required elements

## Development

### Building from Source

```bash
# Prerequisites: Stack (Haskell build tool)
curl sSL https://get.haskellstack.org/ | sh

# Clone and build
git clone https://github.com/tim-br/hymn
cd hymn
stack setup
stack build
stack test
```

### Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Add tests for your changes
4. Ensure all tests pass (`stack test`)
5. Commit your changes (`git commit -am 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

### Adding New Features

The modular design makes it easy to add new functionality:

- **New scales/modes**: Add to `Hymn.Theory`
- **New MusicXML elements**: Extend types in `Hymn.Types` and rendering in `Hymn.Render`
- **New commands**: Add to the CLI in `Hymn.CLI`

<!-- ## Roadmap

- [ ] Minor scales and modes
- [ ] Chord generation and progressions
- [ ] MIDI file support
- [ ] Interactive music generation REPL
- [ ] Web interface for online music generation
- [ ] Advanced rhythm patterns
- [ ] Multiple parts and harmonization
- [ ] Import from MIDI/ABC notation -->

<!-- ## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details. -->

## Acknowledgments

- Inspired by the beauty of functional programming applied to music theory
- MusicXML specification by MakeMusic Inc.
- The Haskell community for excellent libraries and tools

---

*"Music is the pleasure the human mind experiences from counting without being aware that it is counting."* - Gottfried Leibniz
