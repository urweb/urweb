fun selectChar choices current =
    List.mapX (fn (ch, label) =>
                  <xml><option value={String.str ch} selected={current = Some ch}>{[label]}</option></xml>) choices
