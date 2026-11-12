type item = { name : string }

module Inner = struct
  include Other

  type state = Idle | Saved of item [@@deriving fingerprint]
end
