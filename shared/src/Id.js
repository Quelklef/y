exports.newId_f = namespace => () => {
  if (!(typeof namespace === 'string' && /^[a-zA-Z]+$/.test(namespace)))
    throw Error('Namepsace must be string of 1 or more letters');
  const time = Date.now() + '';
  const rand = (Math.floor(Math.random() * 1e6) + '').padStart(6, '0')
  return `y-${namespace}-${time}-${rand}`;
}

exports.parseId_f = just => nothing => caseMaybeOf => maybeNamespace => string => {
  const formatOk = !!/^y-[a-zA-Z]+-[0-9]+-[0-9]{6}$/.test(string);
  const namespaceOk = caseMaybeOf(maybeNamespace)(true)(namespace => string.split('-') === namespace);
  return formatOk && namespaceOk ? just(string) : nothing;
};
