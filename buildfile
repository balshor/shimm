require 'buildr/scala'

repositories.remote << 'http://www.ibiblio.org/maven2'

define 'hidden-markov-models' do
  project.version = '0.0.1'
  package :jar
end