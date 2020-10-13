cd ..
erl -pa ebin -eval "case make:files([\"./src/tools/mmake.erl\"], [{outdir, \"ebin\"}]) of error -> halt(1); _ -> ok end" -eval "case mmake:all(8) of up_to_date -> halt(0); error -> halt(1) end."


pause