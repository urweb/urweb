math.randomseed(os.time())

p = 0.25
n = 2000

function init(args)
  if args[1] then
    p = tonumber(args[1])
  end
  if args[2] then
    n = tonumber(args[2])
  end
end

function request()
  path = "/Bench/"
  if math.random() < p then
    path = path .. "flush"
  else
    path = path .. "check"
  end
  id = math.random(n)
  path = path .. "/" .. id
  return wrk.format(nil, path)
end
