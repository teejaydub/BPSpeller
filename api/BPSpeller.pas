unit BPSpeller;

{ Delphi-style class interface for BPSpellerApi. }

interface

uses Classes, SysUtils;

type
  ESpellBadLanguage = class(Exception)
  public
    constructor Create; overload;  // supplies default message
  end;

  ESpellException = class(Exception)
  public
    constructor Create; overload;
  end;

// Return True if W is a word, False if it's not.
function SpellCheckWord(W: string): Boolean;

// Return a list of suggestions for replacements for W, a misspelled word.
// The caller is responsible for freeing the returned list.
function SpellSuggestions(W: string): TStringList;

implementation

uses BPSpellerAPI;

var
  speller: Pointer;

function SpellCheckWord(W: string): Boolean;
begin
  case BPSpellerAPI.CheckWord(speller, PChar(W)) of
    BPSPELLER_OK: Result := True;
    BPSPELLER_BAD_WORD: Result := False;

    BPSPELLER_LANG_NOT_SUPPORTED,
    BPSPELLER_LANG_IFACE_ERROR,
    BPSPELLER_LANG_NOT_SET:
      raise ESpellBadLanguage.Create;

    else raise ESpellException.Create;
  end;
end;

{$POINTERMATH ON}

function SpellSuggestions(W: string): TStringList;
var
  suggestions: PPWideChar;
  nextSuggestion: PWideChar;
  numSuggestions: Integer;
  i: Integer;
begin
  Result := TStringList.Create;

  case BPSpellerAPI.GetSuggestions(speller, PChar(W), suggestions, numSuggestions) of
    BPSPELLER_OK,
    BPSPELLER_BAD_WORD: begin
      for i := 0 to numSuggestions - 1 do begin
        nextSuggestion := suggestions[i];
        if nextSuggestion[0] <> #0 then
          Result.Add(nextSuggestion);
      end;
    end;

    BPSPELLER_LANG_NOT_SUPPORTED,
    BPSPELLER_LANG_IFACE_ERROR,
    BPSPELLER_LANG_NOT_SET:
      raise ESpellBadLanguage.Create;

    else raise ESpellException.Create;
  end;
end;

{ ESpellBadLanguage }

constructor ESpellBadLanguage.Create;
begin
  inherited Create('Language problem in spell checker');
end;

{ ESpellException }

constructor ESpellException.Create;
begin
  inherited Create('Internal error in spell checker');
end;

initialization

  speller := InitializeSpeller;
  SetSpellerLanguage(speller, 'EN');

finalization

  FreeSpeller(speller);

end.
